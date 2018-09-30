<?php
require_once 'files/Controller/Admin.php';
class Controller_Admin_Dokumenty extends Controller_Admin
{
    public function __construct()
    {
        Permissions::checkError('dokumenty', P_OWNED);
    }

    public function view($request)
    {
        if ($request->post('action') == 'upload' && $request->files()) {
            $fileUpload = $request->files('file')['tmp_name'];
            $fileName = $request->files('file')['name'];
            $fileName = str_replace(
                ['#', '$', '%', '&', '^', '*', '?'],
                ['No.', 'Dolar', 'Procento', 'And', ''],
                $fileName
            );

            $path = 'upload/' . time() . '.' . pathinfo($fileName, PATHINFO_EXTENSION);

            if (!$request->post('name')) {
                $request->post('name', $fileName);
            }

            if (!move_uploaded_file($fileUpload, $path)) {
                $this->redirect('/admin/dokumenty', 'Soubor se nepodařilo nahrát...');
                return;
            }

            chmod($path, 0666);
            DBDokumenty::addDokument(
                $path,
                $request->post('name'),
                $fileName,
                $request->post('kategorie'),
                User::getUserID()
            );
            $this->redirect('/admin/dokumenty', 'Soubor byl úspěšně nahrán');
            return;
        }

        $data = Permissions::check('dokumenty', P_ADMIN)
            ? DBDokumenty::getDokumenty()
            : DBDokumenty::getDokumentyByAuthor(User::getUserID());

        $data = array_map(
            function ($item) {
                return [
                    'buttons' => $this->editLink('/admin/dokumenty/edit/' . $item['d_id'])
                        . '&nbsp;&nbsp;'
                        . $this->removeLink('/admin/dokumenty/remove/' . $item['d_id']),
                    'link' => '<a href="/member/download?id=' . $item['d_id'] . '">' . $item['d_name'] . '</a>',
                    'name' => $item['d_filename'],
                    'category' => Settings::$documentTypes[$item['d_kategorie']],
                    'by' => $item['u_jmeno'] . ' ' . $item['u_prijmeni']
                ];
            },
            $data
        );
        $this->render(
            'files/View/Admin/Dokumenty/Overview.inc',
            ['data' => $data, 'showMenu' => !TISK]
        );
    }

    public function edit($request)
    {
        $id = $request->getId();
        if (!$id || !($data = DBDokumenty::getSingleDokument($id))) {
            $this->redirect('/admin/dokumenty', 'Dokument s takovým ID neexistuje');
        }
        Permissions::checkError('dokumenty', P_OWNED, $data['d_kdo']);

        if ($request->post('newname')) {
            DBDokumenty::editDokument($id, $request->post('newname'));
            $this->redirect('/admin/dokumenty', 'Příspěvek úspěšně upraven');
        }
        $this->render(
            'files/View/Admin/Dokumenty/Form.inc',
            ['name' => $data['d_name']]
        );
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
            $this->redirect('/admin/dokumenty', 'Dokument odebrán');
        }

        $this->render(
            'files/View/Admin/RemovePrompt.inc',
            [
                'header' => 'Správa dokumentů',
                'prompt' => 'Opravdu chcete odstranit dokument:',
                'returnURI' => $request->getReferer() ?: '/admin/dokumenty',
                'data' => [[
                    'id' => $id,
                    'text' => DBDokumenty::getDokumentName($id)
                ]]
            ]
        );
    }
}
