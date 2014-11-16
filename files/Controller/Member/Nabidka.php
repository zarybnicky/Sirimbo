<?php
require_once 'files/Controller/Member.php';
class Controller_Member_Nabidka extends Controller_Member
{
    public function __construct() {
        Permissions::checkError('nabidka', P_VIEW);
    }
    
    public function view($id = null) {
        $this->redirect()->setMessage($this->_processPost());

        $data = array_map(
            function ($data) {
                $items = array_map(
                    function ($item) use ($data) {
                        return array(
                            'id' => $item['u_id'],
                            'fullName' => $item['u_jmeno'] . ' ' . $item['u_prijmeni'],
                            'hourCount' => $item['ni_pocet_hod'],
                            'canDelete' =>
                                (!$data['n_lock']
                                 && Permissions::check('nabidka', P_MEMBER)
                                 && ($item['p_id'] === User::getParID()
                                     || Permissions::check('nabidka', P_OWNED, $data['n_trener']))),
                            'deleteTicket' => $item['p_id'] . '-' . $data['n_id']
                        );
                    },
                    DBNabidka::getNabidkaItem($data['n_id'])
                );

                $obsazeno = array_reduce(
                    $items,
                    function ($carry, $item) {
                        return $carry + $item['hourCount'];
                    },
                    0
                );

                return array(
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
                    'items' => $items
                );
            },
            array_filter(
                DBNabidka::getNabidka(),
                function ($item) {
                    return $item['n_visible'];
                }
            )
        );

        if (empty($data)) {
            $this->render(
                'files/View/Empty.inc',
                array(
                    'nadpis' => 'Nabídka tréninků',
                    'notice' => 'Žádná nabídka k dispozici'
                )
            );
            return;
        }

        $this->render(
            'files/View/Member/Nabidka/Overview.inc',
            array(
                'data' => $data
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
