<?php
namespace Olymp\Controller\Admin;

class Dokumenty
{
    public static function list()
    {
        \Permissions::checkError('dokumenty', P_OWNED);
        $data = array_map(
            fn($item) => [
                'buttons' => \Buttons::document($item['d_id']),
                'link' => '<a href="/member/download?id=' . $item['d_id'] . '">' . $item['d_name'] . '</a>',
                'name' => $item['d_filename'],
                'category' => \Settings::$documentTypes[$item['d_kategorie']],
                'by' => $item['u_jmeno'] . ' ' . $item['u_prijmeni']
            ],
            \Permissions::check('dokumenty', P_ADMIN)
            ? \DBDokumenty::getDokumenty()
            : \DBDokumenty::getDokumentyByAuthor(\Session::getUser()->getId())
        );
        new \RenderHelper('files/View/Admin/Dokumenty/Overview.inc', [
            'header' => 'Správa dokumentů',
            'data' => $data,
        ]);
    }

    public static function listPost()
    {
        \Permissions::checkError('dokumenty', P_OWNED);
        if (empty($_FILES)) {
            return new \RedirectHelper('/admin/dokumenty');
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
            new \MessageHelper('danger', 'Soubor se nepodařilo nahrát.');
            return new \RedirectHelper('/admin/dokumenty');
        }

        chmod($path, 0666);
        \DBDokumenty::addDokument(
            $path,
            $_POST['name'],
            $fileName,
            $_POST['kategorie'],
            \Session::getUser()->getId()
        );
        new \MessageHelper('success', 'Soubor byl úspěšně nahrán');
        return new \RedirectHelper('/admin/dokumenty');
    }

    public static function edit($id)
    {
        \Permissions::checkError('dokumenty', P_OWNED);
        if (!$data = \DBDokumenty::getSingleDokument($id)) {
            new \MessageHelper('warning', 'Dokument s takovým ID neexistuje');
            new \RedirectHelper('/admin/dokumenty');
        }
        \Permissions::checkError('dokumenty', P_OWNED, $data['d_kdo']);
        new \RenderHelper('files/View/Admin/Dokumenty/Form.inc', [
            'header' => 'Správa dokumentů',
            'name' => $data['d_name']
        ]);
    }

    public static function editPost($id)
    {
        \Permissions::checkError('dokumenty', P_OWNED);
        if (!$data = \DBDokumenty::getSingleDokument($id)) {
            new \MessageHelper('warning', 'Dokument s takovým ID neexistuje');
            new \RedirectHelper('/admin/dokumenty');
        }
        \Permissions::checkError('dokumenty', P_OWNED, $data['d_kdo']);
        \DBDokumenty::editDokument($id, $_POST['newname']);
        new \MessageHelper('success', 'Dokument upraven');
        new \RedirectHelper('/admin/dokumenty');
    }

    public static function remove($id)
    {
        \Permissions::checkError('dokumenty', P_OWNED);
        new \RenderHelper('files/View/Admin/RemovePrompt.inc', [
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
        new \RedirectHelper('/admin/dokumenty');
    }
}
