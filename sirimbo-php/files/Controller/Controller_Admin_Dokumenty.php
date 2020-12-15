<?php
class Controller_Admin_Dokumenty
{
    public function view($request)
    {
        \Permissions::checkError('dokumenty', P_OWNED);
        if ($_POST['action'] == 'upload' && !empty($_FILES)) {
            $fileUpload = $_FILES['file']['tmp_name'];
            $fileName = $_FILES['file']['name'];
            $fileName = str_replace(
                ['#', '$', '%', '&', '^', '*', '?'],
                ['No.', 'Dolar', 'Procento', 'And', ''],
                $fileName
            );

            $path = UPLOADS . '/' . time() . '.' . pathinfo($fileName, PATHINFO_EXTENSION);

            if (!$_POST['name']) {
                $_POST['name'] = $fileName;
            }

            if (!move_uploaded_file($fileUpload, $path)) {
                new \MessageHelper('danger', 'Nepodařilo se soubor nahrát.');
                return new \RedirectHelper('/admin/dokumenty');
            }

            chmod($path, 0666);
            \DBDokumenty::addDokument(
                $path,
                $_POST['name'],
                $fileName,
                $_POST['kategorie'],
                \Session::getUserID()
            );
            new \MessageHelper('success', 'Soubor byl nahrán úspěšně');
            return new \RedirectHelper('/admin/dokumenty');
        }

        $data = \Permissions::check('dokumenty', P_ADMIN)
            ? \DBDokumenty::getDokumenty()
            : \DBDokumenty::getDokumentyByAuthor(\Session::getUserID());

        $data = array_map(fn($item) => [
            'buttons' => new EditLinkHelper('/admin/dokumenty/edit/' . $item['d_id'])
            . '&nbsp;&nbsp;'
            . new RemoveLinkHelper('/admin/dokumenty/remove/' . $item['d_id']),
            'link' => '<a href="/member/download?id=' . $item['d_id'] . '">' . $item['d_name'] . '</a>',
            'name' => $item['d_filename'],
            'category' => \Settings::$documentTypes[$item['d_kategorie']],
            'by' => $item['u_jmeno'] . ' ' . $item['u_prijmeni']
        ], $data);
        new \RenderHelper('files/View/Admin/Dokumenty/Overview.inc', [
            'header' => 'Správa dokumentů',
            'data' => $data,
        ]);
    }

    public function edit($request)
    {
        \Permissions::checkError('dokumenty', P_OWNED);
        if (!$id = $request->getId()) {
            new \MessageHelper('warning', 'Dokument s takovým ID neexistuje');
            new \RedirectHelper('/admin/dokumenty');
        }
        if (!$data = \DBDokumenty::getSingleDokument($id)) {
            new \MessageHelper('warning', 'Dokument s takovým ID neexistuje');
            new \RedirectHelper('/admin/dokumenty');
        }
        \Permissions::checkError('dokumenty', P_OWNED, $data['d_kdo']);

        if ($_POST['newname']) {
            \DBDokumenty::editDokument($id, $_POST['newname']);
            new \MessageHelper('success', 'Dokument upraven');
            new \RedirectHelper('/admin/dokumenty');
        }
        new \RenderHelper('files/View/Admin/Dokumenty/Form.inc', [
            'header' => 'Správa dokumentů',
            'name' => $data['d_name']
        ]);
    }

    public function remove($request)
    {
        \Permissions::checkError('dokumenty', P_OWNED);
        if (!$request->getId()) {
            new \RedirectHelper('/admin/dokumenty');
        }
        $id = $request->getId();

        if ($_POST['action'] == 'confirm') {
            $data = \DBDokumenty::getSingleDokument($id);
            if (!\Permissions::check('dokumenty', P_OWNED, $data['d_kdo'])) {
                throw new AuthorizationException("Máte nedostatečnou autorizaci pro tuto akci!");
            }
            unlink($data['d_path']);
            \DBDokumenty::removeDokument($id);
            new \RedirectHelper('/admin/dokumenty');
        }

        new \RenderHelper('files/View/Admin/RemovePrompt.inc', [
            'header' => 'Správa dokumentů',
            'prompt' => 'Opravdu chcete odstranit dokument:',
            'returnURI' => $request->getReferer() ?: '/admin/dokumenty',
            'data' => [[
                'id' => $id,
                'text' => \DBDokumenty::getDokumentName($id)
            ]]
        ]);
    }
}
