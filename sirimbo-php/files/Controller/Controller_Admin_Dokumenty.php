<?php
class Controller_Admin_Dokumenty extends Controller_Abstract
{
    public function __construct($request)
    {
        parent::__construct($request);
        Permissions::checkError('dokumenty', P_OWNED);
    }

    public function view($request)
    {
        if ($request->post('action') == 'upload' && !empty($_FILES)) {
            $fileUpload = $_FILES['file']['tmp_name'];
            $fileName = $_FILES['file']['name'];
            $fileName = str_replace(
                ['#', '$', '%', '&', '^', '*', '?'],
                ['No.', 'Dolar', 'Procento', 'And', ''],
                $fileName
            );

            $path = UPLOADS . '/' . time() . '.' . pathinfo($fileName, PATHINFO_EXTENSION);

            if (!$request->post('name')) {
                $request->post('name', $fileName);
            }

            if (!move_uploaded_file($fileUpload, $path)) {
                $this->redirect()->danger('Nepodařilo se soubor nahrát.');
                return $this->redirect('/admin/dokumenty');
            }

            chmod($path, 0666);
            DBDokumenty::addDokument(
                $path,
                $request->post('name'),
                $fileName,
                $request->post('kategorie'),
                Session::getUserID()
            );
            $this->redirect()->success('Soubor byl nahrán úspěšně');
            return $this->redirect('/admin/dokumenty');
        }

        $data = Permissions::check('dokumenty', P_ADMIN)
            ? DBDokumenty::getDokumenty()
            : DBDokumenty::getDokumentyByAuthor(Session::getUserID());

        $data = array_map(fn($item) => [
            'buttons' => new EditLinkHelper('/admin/dokumenty/edit/' . $item['d_id'])
            . '&nbsp;&nbsp;'
            . new RemoveLinkHelper('/admin/dokumenty/remove/' . $item['d_id']),
            'link' => '<a href="/member/download?id=' . $item['d_id'] . '">' . $item['d_name'] . '</a>',
            'name' => $item['d_filename'],
            'category' => Settings::$documentTypes[$item['d_kategorie']],
            'by' => $item['u_jmeno'] . ' ' . $item['u_prijmeni']
        ], $data);
        new \RenderHelper('files/View/Admin/Dokumenty/Overview.inc', [
            'header' => 'Správa dokumentů',
            'data' => $data,
        ]);
    }

    public function edit($request)
    {
        if (!$id = $request->getId()) {
            $this->redirect()->warning('Dokument s takovým ID neexistuje');
            $this->redirect('/admin/dokumenty');
        }
        if (!$data = DBDokumenty::getSingleDokument($id)) {
            $this->redirect()->warning('Dokument s takovým ID neexistuje');
            $this->redirect('/admin/dokumenty');
        }
        Permissions::checkError('dokumenty', P_OWNED, $data['d_kdo']);

        if ($request->post('newname')) {
            DBDokumenty::editDokument($id, $request->post('newname'));
            $this->redirect()->success('Dokument upraven');
            $this->redirect('/admin/dokumenty');
        }
        new \RenderHelper('files/View/Admin/Dokumenty/Form.inc', [
            'header' => 'Správa dokumentů',
            'name' => $data['d_name']
        ]);
    }

    public function remove($request)
    {
        if (!$request->getId()) {
            $this->redirect('/admin/dokumenty');
        }
        $id = $request->getId();

        if ($request->post('action') == 'confirm') {
            $data = DBDokumenty::getSingleDokument($id);
            if (!Permissions::check('dokumenty', P_OWNED, $data['d_kdo'])) {
                throw new AuthorizationException("Máte nedostatečnou autorizaci pro tuto akci!");
            }
            unlink($data['d_path']);
            DBDokumenty::removeDokument($id);
            $this->redirect('/admin/dokumenty');
        }

        new \RenderHelper('files/View/Admin/RemovePrompt.inc', [
            'header' => 'Správa dokumentů',
            'prompt' => 'Opravdu chcete odstranit dokument:',
            'returnURI' => $request->getReferer() ?: '/admin/dokumenty',
            'data' => [[
                'id' => $id,
                'text' => DBDokumenty::getDokumentName($id)
            ]]
        ]);
    }
}
