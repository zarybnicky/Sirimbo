<?php
namespace Olymp\Controller\Admin;

class GalerieDirectory
{
    public static function list($id)
    {
        \Permissions::checkError('galerie', P_OWNED);
        $data = \Database::querySingle("SELECT * FROM galerie_dir WHERE gd_id='?'", $id);
        if (!$data) {
            \Message::warning('Složka s takovým ID neexistuje');
            \Redirect::to('/galerie');
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
        return self::displayForm('add');
    }

    public static function addPost()
    {
        \Permissions::checkError('galerie', P_OWNED);
        $form = self::checkData();
        if (!$form->isValid()) {
            \Message::warning($form->getMessages());
            return self::displayForm('add');
        }
        $parent = \Database::querySingle("SELECT * FROM galerie_dir WHERE gd_id='?'", $_POST['parent']);
        $dirPath = $parent['gd_path'] . DIRECTORY_SEPARATOR . Galerie::sanitizePathname($_POST['name']);
        mkdir($dirPath, 0777, true);
        \Database::query(
            "INSERT INTO galerie_dir
            (gd_name,gd_id_rodic,gd_level,gd_hidden,gd_path) VALUES
            ('?','?','?','?','?')",
            $_POST['name'],
            $parent['gd_id'],
            $parent['gd_level'] + 1,
            $_POST['hidden'] ? '1' : '0',
            $dirPath
        );
        \Redirect::to('/galerie');
    }

    public static function edit($id)
    {
        \Permissions::checkError('galerie', P_OWNED);
        $data = \Database::querySingle("SELECT * FROM galerie_dir WHERE gd_id='?'", $id);
        if (!$data) {
            \Message::warning('Taková složka neexistuje');
            \Redirect::to('/galerie');
        }
        $_POST['name'] = $data['gd_name'];
        $_POST['parent'] = $data['gd_id_rodic'];
        $_POST['hidden'] = $data['gd_hidden'] ? '1' : '0';
        return self::displayForm('edit');
    }

    public static function editPost($id)
    {
        \Permissions::checkError('galerie', P_OWNED);
        $data = \Database::querySingle("SELECT * FROM galerie_dir WHERE gd_id='?'", $id);
        if (!$data) {
            \Message::warning('Taková složka neexistuje');
            \Redirect::to('/galerie');
        }
        $form = self::checkData();
        if (!$form->isValid()) {
            \Message::warning($form->getMessages());
            return self::displayForm('edit');
        }
        $parent = \Database::querySingle("SELECT * FROM galerie_dir WHERE gd_id='?'", $_POST['parent']);
        $newPath = $parent['gd_path'] . DIRECTORY_SEPARATOR . Galerie::sanitizePathname(
            Galerie::getCanonicalName($_POST['name'])
        );
        if ($data['gd_path'] != $newPath) {
            if (file_exists(GALERIE . DIRECTORY_SEPARATOR . $newPath)) {
                \Message::danger('V dané nadsložce už existuje složka se stejným názvem.');
                \Redirect::to('/galerie/directory/edit/' . $id);
            }
            rename(
                GALERIE . DIRECTORY_SEPARATOR . $data['gd_path'],
                GALERIE . DIRECTORY_SEPARATOR . $newPath
            );
            rename(
                GALERIE_THUMBS . DIRECTORY_SEPARATOR . $data['gd_path'],
                GALERIE_THUMBS . DIRECTORY_SEPARATOR . $newPath
            );
            \Database::query(
                "UPDATE galerie_foto SET gf_path=REPLACE(gf_path,'?','?') WHERE gf_id_rodic='?'",
                $data['gd_path'],
                $newPath,
                $id,
            );
            $data['gd_path'] = $newPath;
        }
        \Database::query(
            "UPDATE galerie_dir
            SET gd_name='?',gd_id_rodic='?',gd_level='?',gd_hidden='?',gd_path='?'
            WHERE gd_id='?'",
            $_POST['name'],
            $parent['gd_id'],
            $parent['gd_level'] + 1,
            $_POST['hidden'] ? '1' : '0',
            $data['gd_path'],
            $id,
        );
        \Redirect::to('/galerie');
    }

    public static function remove($id)
    {
        \Permissions::checkError('galerie', P_OWNED);
        $item = \Database::querySingle("SELECT * FROM galerie_dir WHERE gd_id='?'", $id);
        \Render::twig('RemovePrompt.twig', [
            'header' => 'Správa galerie',
            'prompt' => 'Opravdu chcete odstranit složky se všemi podsložkami a fotkami:',
            'returnURI' => $_SERVER['HTTP_REFERER'] ?? '/galerie',
            'data' => [['id' => $item['gd_id'], 'text' => $item['gd_name']]]
        ]);
    }

    public static function removePost($id)
    {
        \Permissions::checkError('galerie', P_OWNED);
        $data = \Database::querySingle("SELECT * FROM galerie_dir WHERE gd_id='?'", $id);
        \Database::query("DELETE FROM galerie_dir WHERE gd_id='?'", $id);
        \Database::query("DELETE FROM galerie_dir WHERE gd_id_rodic='?'", $id);
        \Database::query("DELETE FROM galerie_foto WHERE gf_id_rodic='?'", $id);
        if ($data['gd_path']) {
            Galerie::rrmdir(GALERIE . DIRECTORY_SEPARATOR . $data['gd_path']);
            Galerie::rrmdir(GALERIE_THUMBS . DIRECTORY_SEPARATOR . $data['gd_path']);
        }
        \Redirect::to('/galerie');
    }

    private static function displayForm($action)
    {
        $dirs = array_map(
            fn($item) => [
                'id' => $item['gd_id'],
                'text' => str_repeat('&nbsp;&nbsp;', $item['gd_level'] - 1) . $item['gd_name']
            ],
            \DBGalerie::getDirs()
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
        $parent = \Database::querySingle("SELECT * FROM galerie_dir WHERE gd_id='?'", $_POST['parent']);
        $form->checkBool($parent, 'Zadaná nadsložka není platná');
        return $form;
    }
}