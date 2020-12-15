<?php
class Controller_Admin_Galerie_Directory
{
    public function view($request)
    {
        \Permissions::checkError('galerie', P_OWNED);
        $id = $request->getId();
        if (!\DBGalerie::getSingleDir($id)) {
            new \MessageHelper('warning', 'Složka s takovým ID neexistuje');
            new \RedirectHelper('/admin/galerie');
        }

        new \RenderHelper('files/View/Admin/Galerie/DisplayDirectory.inc', [
            'header' => 'Správa fotogalerie',
            'id' => $id,
            'files' => array_map(
                fn($item) => [
                    'id' => $item['gf_id'],
                    'name' => $item['gf_name'],
                    'thumbnailURI' => '/galerie/thumbnails/' . $item['gf_path']
                ],
                \DBGalerie::getFotky($id)
            )
        ]);
    }

    public function add()
    {
        \Permissions::checkError('galerie', P_OWNED);
        if (!$_POST) {
            return static::displayForm('add');
        }
        $form = static::checkData();
        if (!$form->isValid()) {
            new \MessageHelper('warning', $form->getMessages());
            return static::displayForm('add');
        }
        $parent = \DBGalerie::getSingleDir($_POST['parent']);
        $dirPath = $parent['gd_path'] . DIRECTORY_SEPARATOR . sanitizePathname($_POST['name']);
        mkdir($dirPath, 0777, true);

        \DBGalerie::addDir(
            $_POST['name'],
            $parent['gd_id'],
            $parent['gd_level'] + 1,
            $_POST['hidden'] ? '1' : '0',
            $dirPath
        );
        new \RedirectHelper('/admin/galerie');
    }

    public function edit($request)
    {
        \Permissions::checkError('galerie', P_OWNED);
        $id = $request->getId();
        if (!$id) {
            new \MessageHelper('warning', 'Není možné upravit hlavní složku');
            new \RedirectHelper('/admin/galerie');
        }
        if (!($data = \DBGalerie::getSingleDir($id))) {
            new \MessageHelper('warning', 'Taková složka neexistuje');
            new \RedirectHelper('/admin/galerie');
        }
        if (!$_POST) {
            $_POST['name'] = $data['gd_name'];
            $_POST['parent'] = $data['gd_id_rodic'];
            $_POST['hidden'] = $data['gd_hidden'] ? '1' : '0';
            return static::displayForm('edit');
        }
        $form = static::checkData();
        if (!$form->isValid()) {
            new \MessageHelper('warning', $form->getMessages());
            return static::displayForm('edit');
        }
        $parent = \DBGalerie::getSingleDir($_POST['parent']);
        $newPath = $parent['gd_path'] . DIRECTORY_SEPARATOR . sanitizePathname(getCanonicalName($_POST['name']));

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
            \DBGalerie::editFotoReplacePath($id, $data['gd_path'], $newPath);
            $data['gd_path'] = $newPath;
        }

        \DBGalerie::editDir(
            $id,
            $_POST['name'],
            $parent['gd_id'],
            $parent['gd_level'] + 1,
            $_POST['hidden'] ? '1' : '0',
            $data['gd_path']
        );
        new \RedirectHelper('/admin/galerie');
    }

    public function remove($request)
    {
        \Permissions::checkError('galerie', P_OWNED);
        if (!$request->getId()) {
            new \RedirectHelper('/admin/galerie');
        }
        $id = $request->getId();

        if ($_POST['action'] == 'confirm') {
            $data = \DBGalerie::getSingleDir($id);
            \DBGalerie::removeDir($id);
            if ($data['gd_path']) {
                rrmdir(GALERIE . DIRECTORY_SEPARATOR . $data['gd_path']);
                rrmdir(GALERIE_THUMBS . DIRECTORY_SEPARATOR . $data['gd_path']);
            }
            new \RedirectHelper('/admin/galerie');
        }

        $item = \DBGalerie::getSingleDir($id);
        new \RenderHelper('files/View/Admin/RemovePrompt.inc', [
            'header' => 'Správa galerie',
            'prompt' => 'Opravdu chcete odstranit složky se všemi podsložkami a fotkami:',
            'returnURI' => $_SERVER['HTTP_REFERER'] ?: '/admin/galerie',
            'data' => [['id' => $item['gd_id'], 'text' => $item['gd_name']]]
        ]);
    }

    private static function displayForm($action)
    {
        $dirs = array_map(
            fn($item) => [
                'id' => $item['gd_id'],
                'text' => str_repeat('&nbsp;&nbsp;', $item['gd_level'] - 1) . $item['gd_name']
            ],
            \DBGalerie::getDirs(true, true)
        );

        return new \RenderHelper('files/View/Admin/Galerie/FormDirectory.inc', [
            'header' => 'Správa fotogalerie',
            'subheader' => ($action == 'add' ? 'Přidat' : 'Upravit') . ' složku',
            'dirs' => $dirs,
            'action' => $action,
            'name' => $_POST['name'] ?: '',
            'parent' => $_POST['parent'] ?: '',
            'hidden' => $_POST['hidden'] ?: ''
        ]);
    }

    protected function checkData( )
    {
        $form = new \Form();
        $form->checkNotEmpty($_POST['name'], 'Název složky nesmí být prázdný', 'name');
        $form->checkBool(
            $_POST['parent'] >= 0
            && is_numeric($_POST['parent'])
            && \DBGalerie::getSingleDir($_POST['parent']),
            'Zadaná nadsložka není platná',
            'parent'
        );
        return $form;
    }
}
