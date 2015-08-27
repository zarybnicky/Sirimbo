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
        switch($request->post('action')) {
        case 'edit':
            $dokumenty = $request->post('dokumenty');
            if ($dokumenty[0]) {
                $this->redirect('/admin/dokumenty/edit/' . $dokumenty[0]);
            }
            break;

        case 'upload':
            if (!$request->files()) {
                break;
            }
            $fileUpload = $request->files('file')['tmp_name'];
            $fileName = $request->files('file')['name'];
            $fileName = str_replace(
                array('#', '$', '%', '&', '^', '*', '?'),
                array('No.', 'Dolar', 'Procento', 'And', ''),
                $fileName
            );

            $path = 'upload/' . time() . '.' . pathinfo($fileName, PATHINFO_EXTENSION);

            if (!$request->post('name')) {
                $request->post('name', $fileName);
            }

            if (move_uploaded_file($fileUpload, $path)) {
                chmod($path, 0666);
                $id = DBDokumenty::addDokument(
                    $path,
                    $request->post('name'),
                    $fileName,
                    $request->post('kategorie'),
                    User::getUserID()
                );
                $this->redirect()->setMessage('Soubor byl úspěšně nahrán');
            } else {
                $this->redirect()->setMessage('Bohužel, zkus to znova :o(');
            }
            $this->redirect('/admin/dokumenty');
            return;

        case 'remove':
            if (!is_array($request->post('dokumenty'))) {
                break;
            }
            $url = '/admin/dokumenty/remove?';
            foreach ($request->post('dokumenty') as $id) {
                $url .= '&u[]=' . $id;
            }
            $this->redirect($url);
            break;
        }
        $data = array_map(
            function ($item) {
                return array(
                    'checkBox' => $this->checkbox('dokumenty[]', $item['d_id'])
                                       ->readonly(Permissions::check('dokumenty', P_OWNED, $item['d_kdo']))
                                       ->render(),
                    'link' => '<a href="/member/download?id=' . $item['d_id'] . '">' . $item['d_name'] . '</a>',
                    'name' => $item['d_filename'],
                    'category' => Settings::$documentTypes[$item['d_kategorie']],
                    'by' => $item['u_jmeno'] . ' ' . $item['u_prijmeni']
                );
            },
            DBDokumenty::getDokumenty()
        );
        $this->render(
            'files/View/Admin/Dokumenty/Overview.inc',
            array(
                'data' => $data,
                'showMenu' => !TISK
            )
        );
    }
    public function edit($request)
    {
        $id = $request->getId();
        if (!$id || !($data = DBDokumenty::getSingleDokument($id))) {
            $this->redirect('/admin/dokumenty', 'Dokument s takovým ID neexistuje');
        }

        if ($request->post('newname')) {
            $newname = $request->post('newname');

            DBDokumenty::editDokument($id, $newname);
            $this->redirect('/admin/dokumenty', 'Příspěvek úspěšně upraven');
        }
        $this->render(
            'files/View/Admin/Dokumenty/Form.inc',
            array(
                'name' => $data['d_name']
            )
        );
    }
    public function remove($request)
    {
        if (!is_array($request->post('data')) && !is_array($request->get('u'))) {
            $this->redirect('/admin/dokumenty');
        }

        if ($request->post('action') == 'confirm') {
            foreach ($request->post('dokumenty') as $id) {
                $data = DBDokumenty::getSingleDokument($id);
                if (Permissions::check('dokumenty', P_OWNED, $data['d_kdo'])) {
                    unlink($data['d_path']);
                    DBDokumenty::removeDokument($id);
                } else {
                    $error = true;
                }
            }
            if (isset($error) && $error) {
                throw new AuthorizationException("Máte nedostatečnou autorizaci pro tuto akci!");
            }
            $this->redirect('/admin/dokumenty', 'Dokumenty odebrány');
        }

        $data = array();
        foreach ($request->get('u') as $id) {
            $data[] = array(
                'id' => $id,
                'text' => DBDokumenty::getDokumentName($id)
            );
        }
        $this->render(
            'files/View/Admin/RemovePrompt.inc',
            array(
                'header' => 'Správa dokumentů',
                'prompt' => 'Opravdu chcete odstranit dokumenty:',
                'returnURI' => $request->getReferer(),
                'data' => $data
            )
        );
    }
}
