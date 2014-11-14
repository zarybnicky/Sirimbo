<?php
require_once 'files/Controller/Member.php';
class Controller_Member_Nabidka extends Controller_Member
{
    public function __construct() {
        Permissions::checkError('nabidka', P_VIEW);
    }
    public function view($id = null) {
        $this->redirect()->setMessage($this->_processPost());

        $nabidky = DBNabidka::getNabidka();
        if (empty($nabidky)) {
            $this->render(
                'files/View/Empty.inc',
                array(
                    'nadpis' => 'Nabídka tréninků',
                    'notice' => 'Žádná nabídka k dispozici'
                )
            );
            return;
        }
        foreach ($nabidky as $key => &$data) {
            if (!$data['n_visible']) {
                unset($nabidky[$key]);
                continue;
            }
            $items = DBNabidka::getNabidkaItem($data['n_id']);
            $obsazeno = 0;
            foreach ($items as &$row) {
                $new_data = array(
                    'id' => $row['u_id'],
                    'fullName' => $row['u_jmeno'] . ' ' . $row['u_prijmeni'],
                    'hourCount' => $row['ni_pocet_hod'],
                    'canDelete' => !$data['n_lock'] && Permissions::check('nabidka', P_MEMBER)
                        && ($row['p_id'] === User::getParID()
                        || Permissions::check('nabidka', P_OWNED, $data['n_trener'])),
                    'deleteTicket' => $row['p_id'] . '-' . $data['n_id']
                );
                $obsazeno += $row['ni_pocet_hod'];
                $row = $new_data;
            }
            $new_data = array(
                'id' => $data['n_id'],
                'fullName' => $data['u_jmeno'] . ' ' . $data['u_prijmeni'],
                'datum' => formatDate($data['n_od'])
                    . ($data['n_od'] != $data['n_do'] ? ' - ' . formatDate($data['n_do']) : ''),
                'canEdit' => Permissions::check('nabidka', P_OWNED, $data['n_trener']),
                'hourMax' => $data['n_max_pocet_hod'],
                'hourTotal' => $data['n_pocet_hod'],
                'hourReserved' => $obsazeno,
                'hourFree' => $data['n_pocet_hod'] - $obsazeno,
                'canAdd' => !$data['n_lock'] && Permissions::check('nabidka', P_MEMBER),
                'items'    => $items
            );
            $data = $new_data;
        }
        $this->render(
            'files/View/Member/Nabidka/Overview.inc',
            array(
                'data' => $nabidky
            )
        );
    }
    private function _checkData($data) {
        $f = new Form();
        $f->checkBool(!$data['n_lock'], 'Tato nabídka je uzamčená', '');
        if (post('hodiny'))
            $f->checkNumeric(post('hodiny'), 'Špatný počet hodin', 'hodiny');
        return $f->isValid() ? null : $f;
    }
    private function _processPost() {
        if (empty($_POST)) {
            return;
        }
        $data = DBNabidka::getSingleNabidka(post('id'));

        if (is_object($f = $this->_checkData($data))) {
            return $f->getMessages();
        } elseif (post('hodiny') > 0) {
            if (!User::getZaplaceno() || (User::getPartnerID() > 0 && !User::getZaplaceno(true))) {
                return 'Buď vy nebo váš partner(ka) nemáte zaplacené členské příspěvky';
            } elseif ($data['n_max_pocet_hod'] > 0
                && (DBNabidka::getNabidkaLessons(post('id'), User::getParID()) + post('hodiny')) > $data['n_max_pocet_hod']) {
                return 'Maximální počet hodin na pár je ' . $data['n_max_pocet_hod'] . '!';
            } elseif (($data['n_pocet_hod'] - DBNabidka::getNabidkaItemLessons(post('id'))) < post('hodiny')) {
                return 'Tolik volných hodin tu není';
            } else {
                DBNabidka::addNabidkaItemLessons(User::getParID(), post('id'), post('hodiny'));
                unset($_POST['hodiny']);
                return 'Hodiny přidány';
            }
        } elseif (post('un_id') !== null) {
            list($u_id, $n_id) = explode('-', post('un_id'));

            if (!DBNabidka::getNabidkaLessons($n_id, $u_id)) {
                return 'Neplatný požadavek!';
            } elseif ($u_id != User::getParID() && !Permissions::check('nabidka', P_OWNED, $data['n_trener'])) {
                return 'Nedostatečná oprávnění!';
            } else {
                DBNabidka::removeNabidkaItem($n_id, $u_id);
                return 'Hodiny odebrány';
            }
        }
    }
}
