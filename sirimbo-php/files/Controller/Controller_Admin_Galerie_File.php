<?php
class Controller_Admin_Galerie_File
{
    public function view($request)
    {
        new \RedirectHelper('/admin/galerie');
    }

    public function edit($request)
    {
        Permissions::checkError('galerie', P_OWNED);
        if (!$id = $request->getId()) {
            new \MessageHelper('warning', 'Takový soubor neexistuje!');
            new \RedirectHelper($request->getReferer());
        }
        if (!$data = DBGalerie::getSingleFoto($id)) {
            new \MessageHelper('warning', 'Takový soubor neexistuje!');
            new \RedirectHelper($request->getReferer());
        }

        if (!$request->post()) {
            $request->post('name', $data['gf_name']);
            $request->post('parent', $data['gf_id_rodic']);
            return $this->displayForm($request, $id);
        }
        $form = $this->checkData($request);
        if (!$form->isValid()) {
            new \MessageHelper('warning', $form->getMessages());
            return $this->displayForm($request, $id);
        }

        $parent = DBGalerie::getSingleDir($request->post('parent'));
        $newPath = sanitizePathname(
            getCanonicalName(
                $parent['gd_path'] . DIRECTORY_SEPARATOR . $request->post('name')
            )
        );

        if ($data['gf_path'] != $newPath) {
            if (file_exists(GALERIE . DIRECTORY_SEPARATOR . $newPath)) {
                new \MessageHelper('danger', 'V dané složce už existuje soubor se stejným názvem.');
                new \RedirectHelper('/admin/galerie/file/edit/' . $id);
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
        new \RedirectHelper('/admin/galerie/directory/' . $request->post('parent'));
    }

    public function remove($request)
    {
        Permissions::checkError('galerie', P_OWNED);
        if (!$request->getId()) {
            new \RedirectHelper('/admin/galerie');
        }
        $id = $request->getId();

        if ($request->post('action') == 'confirm') {
            $item = DBGalerie::getSingleFoto($id);
            DBGalerie::removeFoto($id);
            unlink(GALERIE . DIRECTORY_SEPARATOR . $item['gf_path']);
            unlink(GALERIE_THUMBS . DIRECTORY_SEPARATOR . $item['gf_path']);
            new \RedirectHelper('/admin/galerie');
        }

        $item = DBGalerie::getSingleFoto($id);
        new \RenderHelper('files/View/Admin/RemovePrompt.inc', [
            'header' => 'Správa galerie',
            'prompt' => 'Opravdu chcete odstranit fotografie:',
            'returnURI' => $request->getReferer() ?: '/admin/galerie',
            'data' => [['id' => $id, 'text' => $item['gf_name']]]
        ]);
    }

    public function upload($request)
    {
        Permissions::checkError('galerie', P_OWNED);
        if (!$request->post()) {
            return $this->displayUpload($request);
        }
        $parentId = $request->post('dir');
        if (!is_numeric($parentId) || $parentId < 0) {
            $parentId = 0;
        }
        if (!($parent = DBGalerie::getSingleDir($parentId))) {
            new \MessageHelper('warning', 'Taková složka neexistuje');
            new \RedirectHelper('/admin/galerie/upload');
        }
        $this->_processUpload($parent, $request);
    }

    private function _processUpload($parent, $request)
    {
        $uploadHelper = new \UploadHelper('files');
        $uploadHelper->loadFromPost($request);

        if (!$uploadHelper->hasValidFiles() && $uploadHelper->hasFiles()) {
            new \MessageHelper('warning', $uploadHelper->getErrorMessages());
        }

        $uploader = $uploadHelper->getFilledUploader();
        foreach (Settings::$imageType as $extension) {
            $uploader->addAllowedType($extension);
        }

        $uploader->setOutputDir(GALERIE . DIRECTORY_SEPARATOR . $parent['gd_path']);
        $uploader->save(true, true);
        if ($uploader->hasRefusedFiles()) {
            new \MessageHelper('warning',
                'Počet zamítnutých souborů: ' . count($uploader->getRefusedFiles())
            );
        }

        if (count($uploader->getSavedFiles()) == 0) {
            new \MessageHelper('info', 'Žádné soubory nebyly nahrány!');
            new \RedirectHelper('/admin/galerie/upload');
        }
        $this->_processUploadedFiles($parent, $uploader->getSavedFiles());

        new \MessageHelper('info', 'Počet nahraných souborů: ' . count($uploader->getSavedFiles()));
        new \RedirectHelper('/admin/galerie');
    }

    private function _processUploadedFiles($parent, $files)
    {
        $failCount = 0;
        foreach ($files as $path) {
            if (!checkGetThumbnail($path)) {
                if (is_file($path)) {
                    unlink($path);
                }
                $failCount++;
                continue;
            }
            $path = str_replace(GALERIE . DIRECTORY_SEPARATOR, '', $path);
            $parts = explode(DIRECTORY_SEPARATOR, $path);
            $name = array_pop($parts);

            DBGalerie::addFoto($parent['gd_id'], $path, $name, Session::getUserID());
        }
        if ($failCount > 0) {
            new \MessageHelper('warning',
                'Počet neúspěšně zpracovaných souborů: ' . $failCount
            );
        }
        if (count($files) > $failCount) {
            new \MessageHelper('info', 'Fotky přidány');
            new \RedirectHelper('/admin/galerie');
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
        return new \RenderHelper('files/View/Admin/Galerie/Upload.inc', [
            'header' => 'Správa fotogalerie',
            'subheader' => 'Upload',
            'dirs' => $dirs,
            'dir' => $request->get('dir') ?: '0'
        ]);
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

        new \RenderHelper('files/View/Admin/Galerie/FormFile.inc', [
            'header' => 'Správa fotogalerie',
            'subheader' => 'Upravit soubor',
            'id' => $id,
            'dirs' => $dirs,
            'returnURI' => $request->getReferer(),
            'parent' => $request->post('parent'),
            'name' => $request->post('name')
        ]);
    }

    private function checkData($request): Form
    {
        $form = new Form();

        $form->checkNotEmpty($request->post('name'), 'Zadejte prosím nějaký popis', 'name');
        $form->checkBool(
            $request->post('parent') >= 0 && is_numeric($request->post('parent'))
            && DBGalerie::getSingleDir($request->post('parent')),
            'Zadaná nadsložka není platná',
            'parent'
        );

        return $form;
    }
}
