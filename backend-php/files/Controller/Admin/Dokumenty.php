<?php
namespace Olymp\Controller\Admin;

class Dokumenty
{
    public static $types = [
        '1' => "Schůze,\u{00A0}rady",
        '2' => 'Soutěže',
        '3' => 'Soustředění',
        '0' => 'Ostatní',
    ];

    public static function list()
    {
        \Permissions::checkError('dokumenty', P_OWNED);
        \Render::twig('Admin/Dokumenty.twig', [
            'types' => self::$types,
            'data' => \Permissions::check('dokumenty', P_ADMIN)
            ? \Database::queryArray(
                "SELECT * FROM dokumenty LEFT JOIN users ON d_kdo=u_id ORDER BY d_id DESC"
            )
            : \Database::queryArray(
                "SELECT * FROM dokumenty LEFT JOIN users ON d_kdo=u_id WHERE d_kdo='?' ORDER BY d_id DESC",
                \Session::getUser()->getId(),
            )
        ]);
    }

    public static function listPost()
    {
        \Permissions::checkError('dokumenty', P_OWNED);
        if (empty($_FILES)) {
            \Redirect::to('/admin/dokumenty');
        }
        $fileUpload = $_FILES['file']['tmp_name'];
        $fileName = str_replace(
            ['#', '$', '%', '&', '^', '*', '?'],
            ['No.', 'Dolar', 'Procento', 'And', ''],
            $_FILES['file']['name']
        );
        if (!$_POST['name']) {
            $_POST['name'] = $fileName;
        }

        $path = UPLOADS . '/' . time() . '.' . pathinfo($fileName, PATHINFO_EXTENSION);
        if (!move_uploaded_file($fileUpload, $path)) {
            \Message::danger('Soubor se nepodařilo nahrát.');
            \Redirect::to('/admin/dokumenty');
        }

        chmod($path, 0666);
        \Database::query(
            "INSERT INTO dokumenty (d_path,d_name,d_filename,d_kategorie,d_kdo) VALUES " .
            "('?','?','?','?','?')",
            $path,
            $_POST['name'],
            $fileName,
            $_POST['kategorie'],
            \Session::getUser()->getId()
        );
        \Message::success('Soubor byl úspěšně nahrán');
        \Redirect::to('/admin/dokumenty');
    }

    public static function remove($id)
    {
        \Permissions::checkError('dokumenty', P_OWNED);
        $row = \Database::querySingle("SELECT d_name FROM dokumenty WHERE d_id='?'", $id);
        \Render::twig('RemovePrompt.twig', [
            'header' => 'Správa dokumentů',
            'prompt' => 'Opravdu chcete odstranit dokument:',
            'returnURI' => $_SERVER['HTTP_REFERER'] ?? '/admin/dokumenty',
            'data' => [[
                'id' => $id,
                'text' => $row ? $row['d_name'] : ''
            ]]
        ]);
    }

    public static function removePost($id)
    {
        \Permissions::checkError('dokumenty', P_OWNED);
        $data = \Database::querySingle("SELECT * FROM dokumenty LEFT JOIN users ON d_kdo=u_id WHERE d_id='?'", $id);
        if (!\Permissions::check('dokumenty', P_OWNED, $data['d_kdo'])) {
            throw new \AuthorizationException("Máte nedostatečnou autorizaci pro tuto akci!");
        }
        unlink($data['d_path']);
        \Database::query("DELETE FROM dokumenty WHERE d_id='?'", $id);
        \Redirect::to('/admin/dokumenty');
    }
}
