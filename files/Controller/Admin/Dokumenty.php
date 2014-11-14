<?php
require_once 'files/Controller/Admin.php';
class Controller_Admin_Dokumenty extends Controller_Admin
{
    public function __construct() {
        Permissions::checkError('dokumenty', P_OWNED);
    }
    public function view($id = null) {
        switch(post('action')) {
            case 'edit':
                $dokumenty = post('dokumenty');
                if ($dokumenty[0])
                    $this->redirect('/admin/dokumenty/edit/' . $dokumenty[0]);
                break;

            case 'upload':
                if (empty($_FILES))
                    break;
                $fileUpload = $_FILES['file']['tmp_name'];
                $fileName = $_FILES['file']['name'];
                $fileName = str_replace(
                    array('#', '$', '%', '&', '^', '*', '?'),
                    array('No.', 'Dolar', 'Procento', 'And', ''),
                    $fileName
                );

                $path = 'upload/' . time() . '.' . pathinfo($fileName, PATHINFO_EXTENSION);

                if (!post('name'))
                    post('name', $fileName);

                if (move_uploaded_file($fileUpload, $path)) {
                    chmod($path, 0666);
                    $id = DBDokumenty::addDokument(
                        $path, post('name'), $fileName,
                        post('kategorie'), User::getUserID()
                    );
                    $this->redirect()->setMessage('Soubor byl úspěšně nahrán');

                    $n = new Novinky(User::getUserID());
                    $n->dokumenty()->add('/member/download?id=' . $id, post('name'));
                } else {
                    $this->redirect()->setMessage('Bohužel, zkus to znova :o(');
                }
                $this->redirect('/admin/dokumenty');
                return;

            case 'remove':
                if (!is_array(post('dokumenty')))
                    break;
                $url = '/admin/dokumenty/remove?';
                foreach (post('dokumenty') as $id)
                    $url .= '&u[]=' . $id;
                $this->redirect($url);
                break;
        }
        $data = array_map(
            function($item) {
                return array(
                    'checkBox' => $this->checkbox('dokumenty[]', $item['d_id'])
                                       ->readonly(Permissions::check('dokumenty', P_OWNED, $item['d_kdo'])),
                    'link' => '<a href="/member/download?id=' . $item['d_id'], '">' . $item['d_name'] . '</a>',
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
                'data' => $data
            )
        );
    }
    public function edit($id = null) {
        if (!$id || !($data = DBDokumenty::getSingleDokument($id)))
            $this->redirect('/admin/dokumenty', 'Dokument s takovým ID neexistuje');

        if (!empty($_POST) && post('newname')) {
            $newname = post('newname');

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
    public function remove($id = null) {
        if (!is_array(post('data')) && !is_array(get('u'))) {
            $this->redirect('/admin/dokumenty');
        }

        if (!empty($_POST) && post('action') == 'confirm') {
            foreach (post('dokumenty') as $id) {
                $data = DBDokumenty::getSingleDokument($id);
                if (Permissions::check('dokumenty', P_OWNED, $data['d_kdo'])) {
                    unlink($data['d_path']);
                    DBDokumenty::removeDokument($id);

                    $n = new Novinky(User::getUserID());
                    $n->dokumenty()->remove($data['d_name']);
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
        foreach (get('u') as $id) {
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
                'returnURI' => Request::getReferer(),
                'data' => $data
            )
        );
    }
}
