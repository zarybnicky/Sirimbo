<?php
namespace TKOlomouc\Controller\Admin\Galerie;

use TKOlomouc\Controller\Admin\Galerie;
use TKOlomouc\Utility\Permissions;
use TKOlomouc\Utility\Request;
use TKOlomouc\Utility\Novinky;
use TKOlomouc\Utility\User;
use TKOlomouc\Utility\Form;
use TKOlomouc\Model\DBGalerie;
use TKOlomouc\View\Helper\Upload;
use TKOlomouc\Settings;

class File extends Galerie
{
    public function __construct()
    {
        Permissions::checkError('galerie', P_OWNED);
    }

    public function edit($id = null)
    {
        if (!$id || !($data = DBGalerie::getSingleDir($id))) {
            $this->redirect(Request::getReferer(), 'Takový soubor neexistuje!');
        }
        if (empty($_POST) || is_object($form = $this->checkData())) {
            if (empty($_POST)) {
                post('name', $data['gf_name']);
                post('parent', $data['gf_id_rodic']);
            } else {
                $this->redirect()->setMessage($form->getMessages());
            }
            $this->displayForm($id);
            return;
        }

        $parent = DBGalerie::getSingleDir(post('parent'));
        $newPath = $this->sanitizePathname(
            $this->getCanonicalName(
                $parent['gd_path'] . DIRECTORY_SEPARATOR . post('name')
            )
        );

        if ($data['gf_path'] != $newPath) {
            if (file_exists(GALERIE . DIRECTORY_SEPARATOR . $newPath)) {
                $this->redirect(
                    '/admin/galerie/file/edit/' . $id,
                    'V dané složce už existuje soubor se stejným názvem.'
                );
            }
            rename(
                GALERIE . DIRECTORY_SEPARATOR . $data['gd_path'],
                GALERIE . DIRECTORY_SEPARATOR . $newPath
            );
            rename(
                GALERIE_THUMBS . DIRECTORY_SEPARATOR . $data['gd_path'],
                GALERIE_THUMBS . DIRECTORY_SEPARATOR . $newPath
            );
            $data['gf_path'] = $newPath;
        }

        DBGalerie::editFoto($id, $data['gf_path'], post('parent'), post('name'));
        $this->redirect(
            '/admin/galerie/directory/' . post('parent'),
            'Soubor byl úspěšně upraven'
        );
    }

    public function upload($id = null)
    {
        if (empty($_POST)) {
            $this->displayUpload();
            return;
        }
        $parentId = post('dir');
        if (!is_numeric($parentId) || $parentId < 0) {
            $parentId = 0;
        }
        if (!($parent = DBGalerie::getSingleDir($parentId))) {
            $this->redirect('/admin/galerie/upload', 'Taková složka neexistuje');
        }
        $this->processUpload($parent);
    }

    private function processUpload($parent)
    {
        $uploadHelper = new Upload('files');
        $uploadHelper->loadFromPost();

        if (!$uploadHelper->hasValidFiles() && $uploadHelper->hasFiles()) {
            $this->redirect()->setMessage($uploadHelper->getErrorMessages());
        }

        $uploader = $uploadHelper->getFilledUploadHandler();
        foreach (Settings::$fotoTypes as $extension) {
            $uploader->addAllowedType($extension);
        }

        $uploader->setOutputDir($parent['gd_path']);
        $uploader->save(true, true);
        if ($uploader->hasRefusedFiles()) {
            $this->redirect()->setMessage(
                'Počet zamítnutých souborů: ' . count($uploader->getRefusedFiles())
            );
        }

        if (count($uploader->getSavedFiles()) == 0) {
            $this->redirect(
                '/admin/galerie/upload',
                'Žádné soubory nebyly nahrány!'
            );
        }
        $this->processUploadedFiles($parent, $uploader->getSavedFiles());

        $this->redirect(
            '/admin/galerie',
            'Počet nahraných souborů: ' . count($uploader->getSavedFiles())
        );
    }

    private function processUploadedFiles($parent, $files)
    {
        $failCount = 0;
        foreach ($files as $path) {
            if (!$this->checkGetThumbnail($path)) {
                unlink($path);
                $failCount++;
                continue;
            }
            $path = str_replace(GALERIE . DIRECTORY_SEPARATOR, '', $path);
            $parts = explode(DIRECTORY_SEPARATOR, $path);
            $name = array_pop($parts);

            DBGalerie::addFoto($parent['gd_id'], $path, $name, User::getUserID());
        }
        if ($failCount > 0) {
            $this->redirect()->setMessage(
                'Počet neůspěšně zpracovaných souborů: ' . $failCount
            );
        }
        if (count($files) > $failCount) {
            $news = new Novinky(User::getUserID());
            $news->galerie()->edit($parent['gd_name']);
            $this->redirect('/admin/galerie', 'Fotky přidány');
        }
    }

    private function displayUpload()
    {
        $dirs = DBGalerie::getDirs(true, true);
        foreach ($dirs as &$item) {
            $new_data = array(
                'id'    => $item['gd_id'],
                'text'  => str_repeat('&nbsp;&nbsp;', $item['gd_level'] - 1)
                . $item['gd_name']
            );
            $item = $new_data;
        }
        $this->render(
            'src/application/View/Admin/Galerie/Upload.inc',
            array('dirs' => $dirs)
        );
        return;
    }

    private function displayForm($id)
    {
        $dirs = DBGalerie::getDirs(true, true);
        foreach ($dirs as &$item) {
            $new_data = array(
                'id'    => $item['gd_id'],
                'text'  => str_repeat('&nbsp;&nbsp;', $item['gd_level'] - 1)
                . $item['gd_name']
            );
            $item = $new_data;
        }

        $this->render(
            'src/application/View/Admin/Galerie/FormFile.inc',
            array(
                'id' => $id,
                'dirs' => $dirs,
                'referer' => Request::getReferer()
            )
        );
    }

    private function checkData()
    {
        $form = new Form();

        $form->checkNotEmpty(post('name'), 'Zadejte prosím nějaký popis', 'name');
        $form->checkBool(
            post('parent') >= 0 && is_numeric(post('parent'))
            && DBGalerie::getSingleDir(post('parent')),
            'Zadaná nadsložka není platná', 'parent'
        );

        return $form->isValid() ? array() : $form;
    }
}
