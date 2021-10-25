<?php
namespace Olymp\Controller\Admin;

class GalerieDirectory
{
    public static function list($id)
    {
        \Permissions::checkError('galerie', P_OWNED);
        if (!\DBGalerie::getSingleDir($id)) {
            \Message::warning('Složka s takovým ID neexistuje');
            \Redirect::to('/admin/galerie');
        }
        \Render::twig('Admin/GalerieDirectory.twig', [
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

    public static function add()
    {
        \Permissions::checkError('galerie', P_OWNED);
        return static::displayForm('add');
    }

    public static function addPost()
    {
        \Permissions::checkError('galerie', P_OWNED);
        $form = static::checkData();
        if (!$form->isValid()) {
            \Message::warning($form->getMessages());
            return static::displayForm('add');
        }
        $parent = \DBGalerie::getSingleDir($_POST['parent']);
        $dirPath = $parent['gd_path'] . DIRECTORY_SEPARATOR . Galerie::sanitizePathname($_POST['name']);
        mkdir($dirPath, 0777, true);
        \DBGalerie::addDir(
            $_POST['name'],
            $parent['gd_id'],
            $parent['gd_level'] + 1,
            $_POST['hidden'] ? '1' : '0',
            $dirPath
        );
        \Redirect::to('/admin/galerie');
    }

    public static function edit($id)
    {
        \Permissions::checkError('galerie', P_OWNED);
        if (!($data = \DBGalerie::getSingleDir($id))) {
            \Message::warning('Taková složka neexistuje');
            \Redirect::to('/admin/galerie');
        }
        $_POST['name'] = $data['gd_name'];
        $_POST['parent'] = $data['gd_id_rodic'];
        $_POST['hidden'] = $data['gd_hidden'] ? '1' : '0';
        return static::displayForm('edit');
    }

    public static function editPost($id)
    {
        \Permissions::checkError('galerie', P_OWNED);
        if (!($data = \DBGalerie::getSingleDir($id))) {
            \Message::warning('Taková složka neexistuje');
            \Redirect::to('/admin/galerie');
        }
        $form = static::checkData();
        if (!$form->isValid()) {
            \Message::warning($form->getMessages());
            return static::displayForm('edit');
        }
        $parent = \DBGalerie::getSingleDir($_POST['parent']);
        $newPath = $parent['gd_path'] . DIRECTORY_SEPARATOR . Galerie::sanitizePathname(
            Galerie::getCanonicalName($_POST['name'])
        );
        if ($data['gd_path'] != $newPath) {
            if (file_exists(GALERIE . DIRECTORY_SEPARATOR . $newPath)) {
                \Message::danger('V dané nadsložce už existuje složka se stejným názvem.');
                \Redirect::to('/admin/galerie/directory/edit/' . $id);
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
        \Redirect::to('/admin/galerie');
    }

    public static function remove($id)
    {
        \Permissions::checkError('galerie', P_OWNED);
        $item = \DBGalerie::getSingleDir($id);
        \Render::twig('RemovePrompt.twig', [
            'header' => 'Správa galerie',
            'prompt' => 'Opravdu chcete odstranit složky se všemi podsložkami a fotkami:',
            'returnURI' => $_SERVER['HTTP_REFERER'] ?? '/admin/galerie',
            'data' => [['id' => $item['gd_id'], 'text' => $item['gd_name']]]
        ]);
    }

    public static function removePost($id)
    {
        \Permissions::checkError('galerie', P_OWNED);
        $data = \DBGalerie::getSingleDir($id);
        \DBGalerie::removeDir($id);
        if ($data['gd_path']) {
            Galerie::rrmdir(GALERIE . DIRECTORY_SEPARATOR . $data['gd_path']);
            Galerie::rrmdir(GALERIE_THUMBS . DIRECTORY_SEPARATOR . $data['gd_path']);
        }
        \Redirect::to('/admin/galerie');
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
        \Render::twig('Admin/GalerieFormDirectory.twig', [
            'dirs' => [['id' => '0', 'text' => '---']] + $dirs,
            'action' => $action,
            'name' => $_POST['name'] ?? '',
            'parent' => $_POST['parent'] ?? '',
            'hidden' => $_POST['hidden'] ?? ''
        ]);
    }

    protected static function checkData()
    {
        $form = new \Form();
        $form->checkNotEmpty($_POST['name'], 'Název složky nesmí být prázdný');
        $form->checkBool(
            $_POST['parent'] > 0
            && is_numeric($_POST['parent'])
            && \DBGalerie::getSingleDir($_POST['parent']),
            'Zadaná nadsložka není platná',
        );
        return $form;
    }
}
