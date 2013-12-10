<?php
namespace TKOlomouc\Controller\Admin;

use TKOlomouc\Controller\Admin;
use TKOlomouc\Utility\Permissions;
use TKOlomouc\Utility\Novinky;
use TKOlomouc\Utility\User;
use TKOlomouc\Model\DBDokumenty;
use TKOlomouc\View\Exception\AuthorizationException;
use TKOlomouc\Utility\Request;
use TKOlomouc\Settings;
use TKOlomouc\Utility\Response;
use TKOlomouc\Utility\Form;

class Dokumenty extends Admin
{
    public function __construct()
    {
        Permissions::checkError('dokumenty', P_OWNED);
    }

    public function view($id = null)
    {
        switch(post('action')) {
            case 'edit':
                $dokumenty = post('dokumenty');
                if ($dokumenty[0]) {
                    $this->redirect('/admin/dokumenty/edit/' . $dokumenty[0]);
                }
                break;

            case 'upload':
                $this->processUpload();
                break;

            case 'remove':
                if (is_array(post('dokumenty'))) {
                    $this->redirect(
                        '/admin/dokumenty/remove?' . http_build_query(array('u' => post('dokumenty')))
                    );
                }
                break;
        }

        $this->displayOverview();
    }

    public function edit($id = null)
    {
        if (!$id || !($data = DBDokumenty::getSingleDokument($id))) {
            $this->redirect('/admin/dokumenty', 'Dokument s takovým ID neexistuje');
        }

        if (empty($_POST)) {
            post('name', $data['d_name']);
            post('category', $data['d_kategorie']);

            $this->displayForm();
            return;
        }

        if (is_object($form = $this->checkForm())) {
            Response::setMessage($form->getMessages());

            $this->displayForm();
            return;
        }
        DBDokumenty::editDokument($id, post('name'), post('category'));

        $this->redirect('/admin/dokumenty', 'Soubor byl úspěšně upraven');
    }

    public function remove($id = null)
    {
        if (!is_array(post('data')) && !is_array(get('u'))) {
            $this->redirect('/admin/dokumenty');
        }

        if (!empty($_POST) && post('action') == 'confirm') {
            foreach (post('data') as $id) {
                unlink($data['d_path']);
                DBDokumenty::removeDokument($id);

                $n = new Novinky(User::getUserID());
                $n->dokumenty()->remove($data['d_name']);
            }
            $this->redirect('/admin/dokumenty', 'Dokumenty odebrány');
        }

        $data = DBDokumenty::getMultipleById(get('u'));

        foreach ($data as &$item) {
            $newData = array(
                'id' => $item['d_id'],
                'text' => $item['d_name']
            );
            $item = $newData;
        }
        $this->render(
            'src/application/View/Admin/RemovePrompt.inc',
            array(
                'header' => 'Správa dokumentů',
                'prompt' => 'Opravdu chcete odstranit dokumenty:',
                'returnURL' => Request::getReferer(),
                'data' => $data
            )
        );
    }

    private function displayForm()
    {
        $this->render('src/application/View/Admin/Dokumenty/Display.inc');
    }

    private function displayOverview()
    {
        $data = DBDokumenty::getDokumenty();
        foreach ($data as &$item) {
            $newData = array(
                'id' => $item['d_id'],
                'name' => $item['d_name'],
                'filename' => $item['d_filename'],
                'category' => Settings::$documentTypes[$item['d_kategorie']],
                'fullName' => $item['u_jmeno'] . ' ' . $item['u_prijmeni']
            );
        }
        $this->render(
            'src/application/View/Admin/Dokumenty/Display.inc',
            array(
                'data' => $data
            )
        );
    }

    private function checkForm()
    {
        $form = new Form();

        $form->checkNotEmpty(post('name'), 'Prosím zadejte nějaký popis souboru', 'name');
        $form->checkInArray(post('category'), array('1', '2', '3', '0'), 'Neplatná kategorie');

        return $form->isValid() ? true : $form;
    }

    private function processUpload()
    {
        if (empty($_FILES)) {
            return;
        }
        $fileUpload = $_FILES['file']['tmp_name'];
        $fileName = $_FILES['file']['name'];
        $fileName = str_replace(
            array('#', '$', '%', '&', '^', '*', '?'),
            array('No.', 'Dolar', 'Procento', 'And', ''),
            $fileName
        );

        $path = 'upload/' . time() . '.' . pathinfo($fileName, PATHINFO_EXTENSION);

        if (!post('name')) {
            post('name', $fileName);
        }
        if (move_uploaded_file($fileUpload, $path)) {
            chmod($path, 0666);
            $id = DBDokumenty::addDokument(
                $path,
                post('name'),
                $fileName,
                post('kategorie'),
                User::getUserID()
            );
            $this->redirect()->setMessage('Soubor byl úspěšně nahrán');

            $n = new Novinky(User::getUserID());
            $n->dokumenty()->add('/member/download?id=' . $id, post('name'));
        } else {
            $this->redirect()->setMessage('Bohužel, zkus to znova :o(');
        }
    }
}
