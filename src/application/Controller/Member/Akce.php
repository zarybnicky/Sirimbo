<?php
namespace TKOlomouc\Controller\Member;

use TKOlomouc\Controller\Member;
use TKOlomouc\Utility\Permissions;
use TKOlomouc\Utility\Form;
use TKOlomouc\Model\DBAkce;
use TKOlomouc\Model\DBDokumenty;

class Akce extends Member
{
    public function __construct()
    {
        Permissions::checkError('akce', P_VIEW);
    }

    public function view($id = null)
    {
        if ($id) {
            if (!($data = DBAkce::getSingleAkce($id, true))) {
                $this->redirect('/member/akce', 'Neexistuje žádná taková akce');
            }
            $this->render(
                'src/application/View/Member/Akce/Single.inc',
                array(
                    'data' => $this->getRenderData($data)
                )
            );
            return;
        }
        if (!empty($_POST) && post('id')
            && ($data = DBAkce::getSingleAkce(post('id')))
        ) {
            if (is_object($form = $this->checkData($data, post('action')))) {
                $this->redirect()->setMessage($form->getMessages());
            } elseif (post('action') == 'signup') {
                DBAkce::signUp(
                    User::getUserID(),
                    post('id'), User::getDatumNarozeni()
                );
            } elseif (post('action') == 'signout') {
                DBAkce::signOut(
                    User::getUserID(),
                    post('id')
                );
            }
        }
        $akce = DBAkce::getAkce(true);
        if (empty($akce)) {
            $this->render(
                'src/application/View/Empty.inc',
                array(
                    'nadpis' => 'Klubové akce',
                    'notice' => 'Žádné akce nejsou k dispozici.'
                )
            );
            return;
        }
        foreach ($akce as &$data) {
            $data = $this->getRenderData($data);
        }
        $this->render(
            'src/application/View/Member/Akce/Overview.inc',
            array(
                'akce' => $akce
            )
        );
    }

    private function getRenderData(&$data)
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
        $newData = array(
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
        $newData['signIn'] = $newData['showForm']
            ? !DBAkce::isUserSignedUp($newData['id'], User::getUserID())
            : '';
        return $newData;
    }

    private function checkData($data, $action)
    {
        $f = new Form();

        $f->checkBool(!$data['a_lock'], 'Tato akce je zamčená', '');
        $f->checkInArray($action, array('signup', 'signout'), 'Špatná akce', '');
        $f->checkNumeric(post('id'), 'Špatné ID', '');

        return $f->isValid() ? array() : $f;
    }
}
