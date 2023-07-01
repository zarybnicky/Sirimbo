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
            ? \DBDokumenty::getDokumenty()
            : \DBDokumenty::getDokumentyByAuthor(\Session::getUser()->getId()),
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
        \DBDokumenty::addDokument(
            $path,
            $_POST['name'],
            $fileName,
            $_POST['kategorie'],
            \Session::getUser()->getId()
        );
        \Message::success('Soubor byl úspěšně nahrán');
        \Redirect::to('/admin/dokumenty');
    }

    public static function edit($id)
    {
        \Permissions::checkError('dokumenty', P_OWNED);
        if (!$data = \DBDokumenty::getSingleDokument($id)) {
            \Message::warning('Dokument s takovým ID neexistuje');
            \Redirect::to('/admin/dokumenty');
        }
        \Permissions::checkError('dokumenty', P_OWNED, $data['d_kdo']);
        \Render::twig('Admin/DokumentyForm.twig', [
            'name' => $data['d_name']
        ]);
    }

    public static function editPost($id)
    {
        \Permissions::checkError('dokumenty', P_OWNED);
        if (!$data = \DBDokumenty::getSingleDokument($id)) {
            \Message::warning('Dokument s takovým ID neexistuje');
            \Redirect::to('/admin/dokumenty');
        }
        \Permissions::checkError('dokumenty', P_OWNED, $data['d_kdo']);
        \DBDokumenty::editDokument($id, $_POST['newname']);
        \Message::success('Dokument upraven');
        \Redirect::to('/admin/dokumenty');
    }

    public static function remove($id)
    {
        \Permissions::checkError('dokumenty', P_OWNED);
        \Render::twig('RemovePrompt.twig', [
            'header' => 'Správa dokumentů',
            'prompt' => 'Opravdu chcete odstranit dokument:',
            'returnURI' => $_SERVER['HTTP_REFERER'] ?? '/admin/dokumenty',
            'data' => [[
                'id' => $id,
                'text' => \DBDokumenty::getDokumentName($id)
            ]]
        ]);
    }

    public static function removePost($id)
    {
        \Permissions::checkError('dokumenty', P_OWNED);
        $data = \DBDokumenty::getSingleDokument($id);
        if (!\Permissions::check('dokumenty', P_OWNED, $data['d_kdo'])) {
            throw new \AuthorizationException("Máte nedostatečnou autorizaci pro tuto akci!");
        }
        unlink($data['d_path']);
        \DBDokumenty::removeDokument($id);
        \Redirect::to('/admin/dokumenty');
    }
}
