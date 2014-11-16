<?php
require_once 'files/Controller/Admin/Galerie.php';
class Controller_Admin_Galerie_File extends Controller_Admin_Galerie
{
    public function __construct()
    {
        Permissions::checkError('galerie', P_OWNED);
    }

    public function edit($id = null)
    {
        if(!$id || !($data = DBGalerie::getSingleDir($id))) {
            $this->redirect(Request::getReferer(), 'Takový soubor neexistuje!');
        }
        if(empty($_POST) || is_object($form = $this->_checkData())) {
            if(empty($_POST)) {
                post('name', $data['gf_name']);
                post('parent', $data['gf_id_rodic']);
            } else {
                $this->redirect()->setMessage($form->getMessages());
            }
            $this->_displayForm($id);
            return;
        }

        $parent = DBGalerie::getSingleDir(post('parent'));
        $newPath = $this->_sanitizePathname(
            $this->_getCanonicalName(
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
            $this->_displayUpload();
            return;
        }
        $parentId = post('dir');
        if (!is_numeric($parentId) || $parentId < 0) {
            $parentId = 0;
        }
        if (!($parent = DBGalerie::getSingleDir($parentId))) {
            $this->redirect('/admin/galerie/upload', 'Taková složka neexistuje');
        }
        $this->_processUpload($parent);
    }

    private function _processUpload($parent)
    {
        $uploadHelper = new UploadHelper('files');
        $uploadHelper->loadFromPost();

        if (!$uploadHelper->hasValidFiles() && $uploadHelper->hasFiles()) {
            $this->redirect()->setMessage($uploadHelper->getErrorMessages());
        }

        $uploader = $uploadHelper->getFilledUploader();
        foreach ($this->imageTypes as $extension) {
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
        $this->_processUploadedFiles($parent, $uploader->getSavedFiles());

        $this->redirect(
            '/admin/galerie',
            'Počet nahraných souborů: ' . count($uploader->getSavedFiles())
        );
    }

    private function _processUploadedFiles($parent, $files)
    {
        $failCount = 0;
        foreach ($files as $path) {
            if (!$this->_checkGetThumbnail($path)) {
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

    private function _displayUpload()
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
            'files/View/Admin/Galerie/Upload.inc',
            array('dirs' => $dirs)
        );
        return;
    }

    private function _displayForm($id)
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
            'files/View/Admin/Galerie/FormFile.inc',
            array(
                'id' => $id,
                'dirs' => $dirs,
                'referer' => Request::getReferer()
            )
        );
    }

    private function _checkData()
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
