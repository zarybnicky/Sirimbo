<?php
class Controller_Admin_Galerie_Directory
{
    public function view($request)
    {
        Permissions::checkError('galerie', P_OWNED);
        $id = $request->getId();
        if (!DBGalerie::getSingleDir($id)) {
            new \MessageHelper('warning', 'Složka s takovým ID neexistuje');
            new \RedirectHelper('/admin/galerie');
        }

        new \RenderHelper('files/View/Admin/Galerie/DisplayDirectory.inc', [
            'header' => 'Správa fotogalerie',
            'id' => $id,
            'files' => array_map(
                function ($item) {
                    return [
                        'id' => $item['gf_id'],
                        'name' => $item['gf_name'],
                        'thumbnailURI' => '/galerie/thumbnails/' . $item['gf_path']
                    ];
                },
                DBGalerie::getFotky($id)
            )
        ]);
    }

    public function add($request)
    {
        Permissions::checkError('galerie', P_OWNED);
        if (!$request->post()) {
            return $this->displayForm($request, 'add');
        }
        $form = $this->checkData($request);
        if (!$form->isValid()) {
            new \MessageHelper('warning', $form->getMessages());
            return $this->displayForm($request, 'add');
        }
        $parent = DBGalerie::getSingleDir($request->post('parent'));
        $dirPath = $parent['gd_path'] . DIRECTORY_SEPARATOR
                 . sanitizePathname($request->post('name'));
        mkdir($dirPath, 0777, true);

        DBGalerie::addDir(
            $request->post('name'),
            $parent['gd_id'],
            $parent['gd_level'] + 1,
            $request->post('hidden') ? '1' : '0',
            $dirPath
        );
        new \RedirectHelper('/admin/galerie');
    }

    public function edit($request)
    {
        Permissions::checkError('galerie', P_OWNED);
        $id = $request->getId();
        if (!$id) {
            new \MessageHelper('warning', 'Není možné upravit hlavní složku');
            new \RedirectHelper('/admin/galerie');
        }
        if (!($data = DBGalerie::getSingleDir($id))) {
            new \MessageHelper('warning', 'Taková složka neexistuje');
            new \RedirectHelper('/admin/galerie');
        }
        if (!$request->post()) {
            $request->post('name', $data['gd_name']);
            $request->post('parent', $data['gd_id_rodic']);
            $request->post('hidden', $data['gd_hidden'] ? '1' : '0');
            return $this->displayForm($request, 'edit');
        }
        $form = $this->checkData($request);
        if (!$form->isValid()) {
            new \MessageHelper('warning', $form->getMessages());
            return $this->displayForm($request, 'edit');
        }
        $parent = DBGalerie::getSingleDir($request->post('parent'));
        $newPath = $parent['gd_path'] . DIRECTORY_SEPARATOR
                 . sanitizePathname(
                     getCanonicalName($request->post('name'))
                 );

        if ($data['gd_path'] != $newPath) {
            if (file_exists(GALERIE . DIRECTORY_SEPARATOR . $newPath)) {
                new \MessageHelper('danger', 'V dané nadsložce už existuje složka se stejným názvem.');
                new \RedirectHelper('/admin/galerie/directory/edit/' . $id);
            }
            rename(
                GALERIE . DIRECTORY_SEPARATOR . $data['gd_path'],
                GALERIE . DIRECTORY_SEPARATOR . $newPath
            );
            rename(
                GALERIE_THUMBS . DIRECTORY_SEPARATOR . $data['gd_path'],
                GALERIE_THUMBS . DIRECTORY_SEPARATOR . $newPath
            );
            DBGalerie::editFotoReplacePath($id, $data['gd_path'], $newPath);
            $data['gd_path'] = $newPath;
        }

        DBGalerie::editDir(
            $id,
            $request->post('name'),
            $parent['gd_id'],
            $parent['gd_level'] + 1,
            $request->post('hidden') ? '1' : '0',
            $data['gd_path']
        );
        new \RedirectHelper('/admin/galerie');
    }

    public function remove($request)
    {
        Permissions::checkError('galerie', P_OWNED);
        if (!$request->getId()) {
            new \RedirectHelper('/admin/galerie');
        }
        $id = $request->getId();

        if ($request->post('action') == 'confirm') {
            $data = DBGalerie::getSingleDir($id);
            DBGalerie::removeDir($id);
            if ($data['gd_path']) {
                rrmdir(GALERIE . DIRECTORY_SEPARATOR . $data['gd_path']);
                rrmdir(GALERIE_THUMBS . DIRECTORY_SEPARATOR . $data['gd_path']);
            }
            new \RedirectHelper('/admin/galerie');
        }

        $item = DBGalerie::getSingleDir($id);
        new \RenderHelper('files/View/Admin/RemovePrompt.inc', [
            'header' => 'Správa galerie',
            'prompt' => 'Opravdu chcete odstranit složky se všemi podsložkami a fotkami:',
            'returnURI' => $request->getReferer() ?: '/admin/galerie',
            'data' => [['id' => $item['gd_id'], 'text' => $item['gd_name']]]
        ]);
    }

    private function displayForm($request, $action)
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

        return new \RenderHelper('files/View/Admin/Galerie/FormDirectory.inc', [
            'header' => 'Správa fotogalerie',
            'subheader' => ($action == 'add' ? 'Přidat' : 'Upravit') . ' složku',
            'dirs' => $dirs,
            'action' => $action,
            'name' => $request->post('name') ?: '',
            'parent' => $request->post('parent') ?: '',
            'hidden' => $request->post('hidden') ?: ''
        ]);
    }

    protected function checkData($request)
    {
        $form = new Form();
        $form->checkNotEmpty(
            $request->post('name'),
            'Název složky nesmí být prázdný',
            'name'
        );
        $form->checkBool(
            $request->post('parent') >= 0
            && is_numeric($request->post('parent'))
            && DBGalerie::getSingleDir($request->post('parent')),
            'Zadaná nadsložka není platná',
            'parent'
        );
        return $form;
    }
}
