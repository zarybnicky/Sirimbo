<?php
require_once 'files/Controller/Admin.php';
class Controller_Admin_Nabidka extends Controller_Admin
{
    public function __construct() {
        Permissions::checkError('nabidka', P_OWNED);
    }
    public function view($request) {
        switch(post('action')) {
            case 'save':
                foreach (DBNabidka::getNabidka() as $item) {
                    if ((bool) post($item['n_id']) !== (bool) $item['n_visible']) {
                        DBNabidka::editNabidka(
                            $item['n_id'],
                            $item['n_trener'],
                            $item['n_pocet_hod'],
                            $item['n_max_pocet_hod'],
                            $item['n_od'],
                            $item['n_do'],
                            post($item['n_id']) ? '1' : '0',
                            $item['n_lock']
                        );
                    }
                }
                $this->redirect('/admin/nabidka', 'Nabídky upraveny');
                break;

            case 'remove':
                if (!is_array(post('nabidka')))
                    break;
                foreach (post('nabidka') as $item) {
                    $data = DBNabidka::getSingleNabidka($item);
                    if (!Permissions::check('nabidka', P_OWNED, $data['n_trener'])) {
                        $error = true;
                        continue;
                    }
                    DBNabidka::removeNabidka($item);
                    if (strcmp($data['n_od'], date('Y-m-d')) > 0 && $data['n_visible']) {
                        $od = new Date($data['n_od']);
                        $do = new Date($data['n_do']);
                        $n = new Novinky(User::getUserID());
                        $n->nabidka()->remove(
                            $od->getYear() == $do->getYear()
                            ? ($od->getDay() . '. ' . $od->getMonth() . '.')
                            : $od->getDate(Date::FORMAT_SIMPLIFIED),
                            $do->getDate(Date::FORMAT_SIMPLIFIED),
                            $data['u_jmeno'] . ' ' . $data['u_prijmeni']
                        );
                    }
                }
                if (isset($error) && $error)
                    throw new AuthorizationException("Máte nedostatečnou autorizaci pro tuto akci!");
                $this->redirect('/admin/nabidka', 'Nabídky odebrány');
                break;

            case 'duplicate':
                foreach (post('nabidka') as $oldId) {
                    $data = DBNabidka::getSingleNabidka($oldId);
                    $items = DBNabidka::getNabidkaItem($oldId);

                    $newId = DBNabidka::addNabidka(
                        $data['n_trener'],
                        $data['n_pocet_hod'],
                        $data['n_max_pocet_hod'],
                        $data['n_od'],
                        $data['n_do'],
                        $data['n_visible'],
                        $data['n_lock']
                    );
                    foreach ($items as $item) {
                        DBNabidka::addNabidkaItemLessons(
                            $item['ni_partner'],
                            $newId,
                            $item['ni_pocet_hod']
                        );
                    }
                }
                $this->redirect('/admin/nabidka');
                break;
        }

        $data = array_map(
            function ($item) {
                $isTrainer = Permissions::check('nabidka', P_OWNED, $item['n_trener']);
                $isAdmin = Permissions::check('nabidka', P_ADMIN);
                
                return array(
                    'canEdit' => $isTrainer,
                    'fullName' => $item['u_jmeno'] . ' ' . $item['u_prijmeni'],
                    'date' => (
                        formatDate($item['n_od']) .
                        ($item['n_do'] != $item['n_od']
                         ? ' - ' . formatDate($item['n_do'])
                         : '')
                    ),
                    'links' => (
                        '<a href="/admin/nabidka/edit/' . $item['n_id'] . '">obecné</a>, ' .
                        '<a href="/admin/nabidka/detail/' . $item['n_id'] . '">tréninky</a>'
                    ),
                    'visible' => (
                        $isAdmin ?
                        $this->checkbox($item['n_id'], '1')
                             ->set($item['n_visible'])
                             ->render() :
                        ('&nbsp;' . ($item['n_visible'] ? '&#10003;' : '&#10799;'))
                    ),
                    'checkBox' => (
                        $isTrainer ?
                        (string) $this->checkbox('nabidka[]', $item['n_id']) :
                        '&nbsp;&#10799;'
                    )
                );
            },
            DBNabidka::getNabidka(true)
        );
        $this->render(
            'files/View/Admin/Nabidka/Overview.inc',
            array(
                'showMenu' => !TISK,
                'data' => $data
            )
        );
    }
    public function add($request) {
        if (empty($_POST) || is_object($f = $this->_checkData())) {
            if (!empty($_POST)) {
                $this->redirect()->setMessage($f->getMessages());
            }
            $this->render(
                'files/View/Admin/Nabidka/Form.inc',
                array(
                    'action' => $request->getAction(),
                    'referer' => $request->getReferer(),
                    'returnURI' => $request->getReferer(),
                    'users' => DBUser::getUsersByPermission('nabidka', P_OWNED),
                    'visible' => false
                )
            );
            return;
        }
        Permissions::checkError('nabidka', P_OWNED, post('trener'));

        $od = $this->date('od')->getPost();
        $do = $this->date('do')->getPost();
        if (!$do->isValid() || strcmp((string) $od, (string) $do) > 0)
            $do = $od;

        $visible = (bool) post('visible');
        if (!Permissions::check('nabidka', P_ADMIN) && $visible) {
            $visible = false;
            $this->redirect()->setMessage('Nemáte dostatečná oprávnění ke zviditelnění příspěvku');
        }
        if (!is_numeric(post('max_pocet_hod')))
            post('max_pocet_hod', 0);

        DBNabidka::addNabidka(
            post('trener'), post('pocet_hod'), post('max_pocet_hod'),
            (string) $od, (string) $do, $visible, post('lock') ? 1 : 0
        );

        if ($visible) {
            $trener_data = DBUser::getUserData(post('trener'));
            $trener_name = $trener_data['u_jmeno'] . ' ' . $trener_data['u_prijmeni'];

            $n = new Novinky(User::getUserID());
            $n->nabidka()->add(
                '/member/nabidka',
                $od->getYear() == $do->getYear()
                ? ($od->getDay() . '. ' . $od->getMonth() . '.')
                : $od->getDate(Date::FORMAT_SIMPLIFIED),
                $do->getDate(Date::FORMAT_SIMPLIFIED), $trener_name
            );
        }
        $this->redirect(
            post('referer') ? post('referer') : '/admin/nabidka',
            'Nabídka přidána'
        );
    }
    public function edit($request) {
        $id = $request->getId();
        if (!$id || !($data = DBNabidka::getSingleNabidka($id))) {
            $this->redirect(
                post('referer') ? post('referer') : '/admin/nabidka',
                'Nabídka s takovým ID neexistuje'
            );
        }
        Permissions::checkError('nabidka', P_OWNED, $data['n_trener']);

        if (empty($_POST) || is_object($f = $this->_checkData())) {
            if (empty($_POST)) {
                post('id', $id);
                post('trener', $data['n_trener']);
                post('pocet_hod', $data['n_pocet_hod']);
                post('max_pocet_hod', $data['n_max_pocet_hod']);
                post('od', $data['n_od']);
                post('do', $data['n_do']);
                post('visible', $data['n_visible']);
                post('lock', $data['n_lock']);
            } else {
                $this->redirect()->setMessage($f->getMessages());
            }
            $data = DBNabidka::getSingleNabidka($id);
            $this->render(
                'files/View/Admin/Nabidka/Form.inc',
                array(
                    'action' => $request->getAction(),
                    'referer' => $request->getReferer(),
                    'returnURI' => $request->getReferer(),
                    'users' => DBUser::getUsersByPermission('nabidka', P_OWNED),
                    'visible' => $data['n_visible']
                )
            );
            return;
        }
        $od = $this->date('od')->getPost();
        $do = $this->date('do')->getPost();
        if (!$do->isValid() || strcmp((string) $od, (string) $do) > 0)
            $do = $od;

        $visible = (bool) post('visible');
        $visible_prev = $data['n_visible'];
        if (!Permissions::check('nabidka', P_ADMIN)
            && $visible && !$visible_prev) {
            $visible = false;
            $this->redirect()->setMessage('Nemáte dostatečná oprávnění ke zviditelnění nabídky');
        }
        $items = DBNabidka::getNabidkaItemLessons($id);
        $pocet_hod = post('pocet_hod');
        if ($pocet_hod < $items) {
            $pocet_hod = $items;
            $this->redirect()->setMessage(
                'Obsazených hodin už je víc než jste zadali, ' .
                'nemůžu dál snížit počet hodin'
            );
        }
        $max_lessons = post('max_pocet_hod');
        $max_lessons_old = DBNabidka::getNabidkaMaxItems($id);
        if ($max_lessons < $max_lessons_old && $max_lessons != 0) {
            $max_lessons = $max_lessons_old;
            $this->redirect()->setMessage(
                'Zadaný maximální počet hodin/pár je méně než už je zarezervováno, ' .
                'nemůžu dál snížit maximální počet hodin'
            );
        }
        if (!is_numeric($max_lessons))
            $max_lessons = 0;

        DBNabidka::editNabidka(
            $id, post('trener'), $pocet_hod, $max_lessons,
            (string) $od, (string) $do, $visible,
            post('lock') ? '1' : '0'
        );

        if ($visible) {
            if (!$visible_prev)
                $act = 'add';
            else
                $act = 'edit';
        } elseif (!$visible && $visible_prev && strcmp($data['n_od'], date('Y-m-d')) > 0)
            $act = 'remove';

        if (isset($act)) {
            $trener_data = DBUser::getUserData(post('trener'));
            $n = new Novinky(User::getUserID());
            if ($act == 'remove') {
                $n->nabidka()->$act(
                    $od->getYear() == $do->getYear()
                    ? ($od->getDay() . '. ' . $od->getMonth() . '.')
                    : $od->getDate(Date::FORMAT_SIMPLIFIED),
                    $do->getDate(Date::FORMAT_SIMPLIFIED),
                    $trener_data['u_jmeno'] . ' ' . $trener_data['u_prijmeni']);
            } else {
                $n->nabidka()->$act(
                    '/member/nabidka',
                    $od->getYear() == $do->getYear()
                    ? ($od->getDay() . '. ' . $od->getMonth() . '.')
                    : $od->getDate(Date::FORMAT_SIMPLIFIED),
                    $do->getDate(Date::FORMAT_SIMPLIFIED),
                    $trener_data['u_jmeno'] . ' ' . $trener_data['u_prijmeni']
                );
            }
        }
        $this->redirect(
            post('referer') ? post('referer') : '/admin/nabidka',
            'Nabídka úspěšně upravena'
        );
    }

    private function _checkData() {
        $od = $this->date('od')->getPost();
        $do = $this->date('do')->getPost();
        if (!$do->isValid() || strcmp((string) $od, (string) $do) > 0)
            $do = $od;

        $f = new Form();
        $f->checkNumeric(post('trener'), 'ID trenéra musí být číselné', 'trener');
        $f->checkNumeric(post('pocet_hod'), 'Počet hodin prosím zadejte čísly', 'pocet_hod');
        $f->checkDate((string) $od, 'Zadejte prosím platné datum ("Od")', 'od');
        if ($do->isValid())
            $f->checkDate((string) $do, 'Zadejte prosím platné datum ("Do")', 'do');

        return $f->isValid() ? true : $f;
    }
}
