<?php
require_once 'files/Controller/Member.php';
class Controller_Member_Akce extends Controller_Member
{
    function __construct() {
        Permissions::checkError('akce', P_VIEW);
    }
    function view($id = null) {
        if ($id) {
            $data = DBAkce::getSingleAkce($id, true);
            if (!$data)
                $this->redirect('/member/akce', 'Neexistuje žádná taková akce');

            $data = $this->_getRenderData($data);
            $this->render('files/View/Member/Akce/Single.inc', array('data' => $data));
            return;
        }
        if (!empty($_POST) && post('id') &&
                ($data = DBAkce::getSingleAkce(post('id')))) {
            if (is_object($f = $this->_checkData($data, post('action')))) {
                $this->redirect()->setMessage($f->getMessages());
            } elseif (post('action') == 'signup') {
                DBAkce::signUp(User::getUserID(), post('id'), User::getDatumNarozeni());
            } elseif (post('action') == 'signout') {
                DBAkce::signOut(User::getUserID(), post('id'));
            }
        }
        $akce = DBAkce::getAkce(true);
        if (empty($akce)) {
            $this->render('files/View/Empty.inc', array(
                    'nadpis' => 'Klubové akce',
                    'notice' => 'Žádné akce nejsou k dispozici.')
            );
            return;
        }
        foreach ($akce as &$data) {
            $data = $this->_getRenderData($data);
        }
        $this->render('files/View/Member/Akce/Overview.inc', array('akce' => $akce));
    }
    private function _getRenderData(&$data) {
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
        $new_data['signIn'] = $new_data['showForm'] ? !DBAkce::isUserSignedUp($new_data['id'], User::getUserID()) : '';
        return $new_data;
    }
    private function _checkData($data, $action) {
        $f = new Form();
        $f->checkBool(!$data['a_lock'], 'Tato akce je zamčená', '');
        $f->checkInArray($action, array('signup', 'signout'), 'Špatná akce', '');
        $f->checkNumeric(post('id'), 'Špatné ID', '');

        return $f->isValid() ? true : $f;
    }
}
?>