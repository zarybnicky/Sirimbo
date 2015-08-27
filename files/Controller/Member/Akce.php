<?php
require_once 'files/Controller/Member.php';
class Controller_Member_Akce extends Controller_Member
{
    public function __construct()
    {
        Permissions::checkError('akce', P_VIEW);
    }

    public function view($request)
    {
        $id = $request->getId();
        if ($id) {
            if (!($data = DBAkce::getSingleAkce($id, true))) {
                $this->redirect('/member/akce', 'Neexistuje žádná taková akce');
            }
            $this->render(
                'files/View/Member/Akce/Single.inc',
                array(
                    'data' => $this->_getRenderData($data)
                )
            );
            return;
        }
        if ($request->post('id')
            && ($data = DBAkce::getSingleAkce($request->post('id')))
        ) {
            if (is_object($form = $this->checkData($request, $data, $request->post('action')))) {
                $this->redirect()->setMessage($form->getMessages());
            } elseif ($request->post('action') == 'signup') {
                $date = explode('-', User::getDatumNarozeni());

                DBAkce::signUp(
                    User::getUserID(),
                    $request->post('id'),
                    $date[0]
                );
            } elseif ($request->post('action') == 'signout') {
                DBAkce::signOut(
                    User::getUserID(),
                    $request->post('id')
                );
            }
        }
        $akce = DBAkce::getAkce(true);
        if (empty($akce)) {
            $this->render(
                'files/View/Empty.inc',
                array(
                    'nadpis' => 'Klubové akce',
                    'notice' => 'Žádné akce nejsou k dispozici.'
                )
            );
            return;
        }
        foreach ($akce as &$data) {
            $data = $this->_getRenderData($data);
        }
        $this->render(
            'files/View/Member/Akce/Overview.inc',
            array(
                'akce' => $akce
            )
        );
    }
    private function _getRenderData(&$data)
    {
        $items = DBAkce::getAkceItems($data['a_id']);
        $dokumenty = unserialize($data['a_dokumenty']);
        if (is_array($dokumenty)) {
            foreach ($dokumenty as &$row) {
                $dokument = DBDokumenty::getSingleDokument($row);
                $new_row = array(
                    'id' => $row,
                    'name' => $dokument['d_name']
                );
                $row = $new_row;
            }
        } else {
            $dokumenty = array();
        }
        $new_data = array(
            'id' => $data['a_id'],
            'jmeno' => $data['a_jmeno'],
            'kde' => $data['a_kde'],
            'datum' => formatDate($data['a_od']) .
                (($data['a_od'] != $data['a_do']) ? ' - ' . formatDate($data['a_do']) : ''),
            'kapacita' => $data['a_kapacita'],
            'volno' => $data['a_kapacita'] - count($items),
            'showForm' => Permissions::check('akce', P_MEMBER) && !$data['a_lock'],
            'canEdit' => Permissions::check('akce', P_OWNED),
            'info' => nl2br($data['a_info']),
            'dokumenty' => $dokumenty,
            'items' => $items
        );
        $new_data['signIn'] = $new_data['showForm']
                            ? !DBAkce::isUserSignedUp($new_data['id'], User::getUserID())
                            : '';
        return $new_data;
    }
    private function checkData($request, $data, $action)
    {
        $f = new Form();
        $f->checkBool(!$data['a_lock'], 'Tato akce je zamčená', '');
        $f->checkInArray($action, array('signup', 'signout'), 'Špatná akce', '');
        $f->checkNumeric($request->post('id'), 'Špatné ID', '');

        return $f->isValid() ? array() : $f;
    }
}
?>