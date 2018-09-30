<?php
require_once 'files/Controller/Admin/Galerie.php';
class Controller_Admin_Galerie_File extends Controller_Admin_Galerie
{
    public function __construct()
    {
        Permissions::checkError('galerie', P_OWNED);
    }

    public function edit($request)
    {
        $id = $request->getId();
        if (!$id || !($data = DBGalerie::getSingleFoto($id))) {
            $this->redirect($request->getReferer(), 'Takový soubor neexistuje!');
        }
        if (!$request->post()) {
            $request->post('name', $data['gf_name']);
            $request->post('parent', $data['gf_id_rodic']);
            $this->displayForm($request, $id);
            return;
        }
        if (is_object($form = $this->_checkData())) {
            $this->redirect()->setMessage($form->getMessages());
            $this->displayForm($request, $id);
            return;
        }

        $parent = DBGalerie::getSingleDir($request->post('parent'));
        $newPath = $this->_sanitizePathname(
            $this->_getCanonicalName(
                $parent['gd_path'] . DIRECTORY_SEPARATOR . $request->post('name')
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

        DBGalerie::editFoto($id, $data['gf_path'], $request->post('parent'), $request->post('name'));
        $this->redirect(
            '/admin/galerie/directory/' . $request->post('parent'),
            'Soubor byl úspěšně upraven'
        );
    }

    public function remove($request)
    {
        if (!$request->getId()) {
            $this->redirect('/admin/galerie');
        }
        $id = $request->getId();

        if ($request->post('action') == 'confirm') {
            $item = DBGalerie::getSingleFoto($id);
            DBGalerie::removeFoto($id);
            unlink(GALERIE . DIRECTORY_SEPARATOR . $item['gf_path']);
            unlink(GALERIE_THUMBS . DIRECTORY_SEPARATOR . $item['gf_path']);
            $this->redirect('/admin/galerie', 'Fotografie odebrána');
        }

        $item = DBGalerie::getSingleFoto($id);
        $this->render('files/View/Admin/RemovePrompt.inc', [
            'header' => 'Správa galerie',
            'prompt' => 'Opravdu chcete odstranit fotografie:',
            'returnURI' => $request->getReferer() ?: '/admin/galerie',
            'data' => [['id' => $id, 'text' => $item['gf_name']]]
        ]);
    }

    public function upload($request)
    {
        if (!$request->post()) {
            $this->displayUpload($request);
            return;
        }
        $parentId = $request->post('dir');
        if (!is_numeric($parentId) || $parentId < 0) {
            $parentId = 0;
        }
        if (!($parent = DBGalerie::getSingleDir($parentId))) {
            $this->redirect('/admin/galerie/upload', 'Taková složka neexistuje');
        }
        $this->_processUpload($parent, $request);
    }

    private function _processUpload($parent, $request)
    {
        $uploadHelper = new UploadHelper();
        $uploadHelper->upload('files');
        $uploadHelper->loadFromPost($request);

        if (!$uploadHelper->hasValidFiles() && $uploadHelper->hasFiles()) {
            $this->redirect()->setMessage($uploadHelper->getErrorMessages());
        }

        $uploader = $uploadHelper->getFilledUploader();
        foreach ($this->imageType as $extension) {
            $uploader->addAllowedType($extension);
        }

        $uploader->setOutputDir(GALERIE . DIRECTORY_SEPARATOR . $parent['gd_path']);
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
                if (is_file($path)) {
                    unlink($path);
                }
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
            $this->redirect('/admin/galerie', 'Fotky přidány');
        }
    }

    private function displayUpload($request)
    {
        $dirs = array_map(
            function ($item) {
                return [
                    'id'    => $item['gd_id'],
                    'text'  => str_repeat('&nbsp;&nbsp;', $item['gd_level'] - 1)
                    . $item['gd_name']
                ];
            },
            DBGalerie::getDirs(true, true)
        );
        $this->render(
            'files/View/Admin/Galerie/Upload.inc',
            ['dirs' => $dirs, 'dir' => $request->get('dir') ?: '0']
        );
        return;
    }

    private function displayForm($request, $id)
    {
        $dirs = DBGalerie::getDirs(true, true);
        $dirs = array_map(
            function ($item) {
                return [
                    'id'    => $item['gd_id'],
                    'text'  => str_repeat('&nbsp;&nbsp;', $item['gd_level'] - 1)
                    . $item['gd_name']
                ];
            },
            $dirs
        );

        $this->render(
            'files/View/Admin/Galerie/FormFile.inc',
            [
                'id' => $id,
                'dirs' => $dirs,
                'referer' => $request->getReferer(),
                'parent' => $request->post('parent'),
                'name' => $request->post('name')
            ]
        );
    }

    private function _checkData()
    {
        $form = new Form();

        $form->checkNotEmpty($request->post('name'), 'Zadejte prosím nějaký popis', 'name');
        $form->checkBool(
            $request->post('parent') >= 0 && is_numeric($request->post('parent'))
            && DBGalerie::getSingleDir($request->post('parent')),
            'Zadaná nadsložka není platná', 'parent'
        );

        return $form->isValid() ? [] : $form;
    }
}
