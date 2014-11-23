<?php
require_once 'files/Controller/Admin.php';
class Controller_Admin_Rozpis extends Controller_Admin
{
    public function __construct() {
        Permissions::checkError('rozpis', P_OWNED);
    }
    public function view($request) {
        switch(post('action')) {
            case 'save':
                $items = DBRozpis::getRozpis();
                foreach ($items as $item) {
                    $id = $item['r_id'];
                    if ((bool) post($id) !== (bool) $item['r_visible']
                        && Permissions::check('rozpis', P_OWNED, $item['r_trener'])
                    ) {
                        if (!Permissions::check('rozpis', P_ADMIN) && post($id) && !$item['r_visible']) {
                            $this->redirect()->setMessage('Nemáte dostatečná oprávnění ke zviditelnění rozpisu');
                        } else {
                            DBRozpis::editRozpis(
                                $id, $item['r_trener'], $item['r_kde'],
                                $item['r_datum'], post($id) ? '1' : '0', $item['r_lock'] ? '1' : '0'
                            );
                            $n = new Novinky(User::getUserID());
                            if (!post($id)) {
                                $n->rozpis()->remove(
                                    (new Date($item['r_datum']))->getDate(Date::FORMAT_SIMPLIFIED),
                                    $item['u_jmeno'] . ' ' . $item['u_prijmeni']
                                );
                            } else {
                                $n->rozpis()->add(
                                    '/member/rozpis',
                                    (new Date($item['r_datum']))->getDate(Date::FORMAT_SIMPLIFIED),
                                    $item['u_jmeno'] . ' ' . $item['u_prijmeni']
                                );
                            }
                        }
                    }
                }
                break;

            case 'remove':
                if (!is_array(post('rozpis')))
                    $this->redirect('/admin/rozpis');
                foreach (post('rozpis') as $item) {
                    $trener = DBRozpis::getRozpisTrener($item);
                    $data = DBRozpis::getSingleRozpis($item);

                    if (Permissions::check('rozpis', P_OWNED, $trener['u_id'])) {
                        DBRozpis::removeRozpis($item);
                        if (strcmp($data['r_datum'], date('Y-m-d')) > 0 && $data['r_visible']) {
                            $n = new Novinky(User::getUserID());
                            $n->rozpis()->remove(
                                (new Date($data['r_datum']))->getDate(Date::FORMAT_SIMPLIFIED),
                                $trener['u_jmeno'] . ' ' . $trener['u_prijmeni']
                            );
                        }
                    } else {
                        $error = true;
                    }
                }
                if (isset($error) && $error)
                    throw new AuthorizationException("Máte nedostatečnou autorizaci pro tuto akci!");

                $this->redirect('/admin/rozpis', 'Rozpisy odebrány');
                
            case 'duplicate':
                foreach (post('rozpis') as $oldId) {
                    $data = DBRozpis::getSingleRozpis($oldId);
                    $items = DBRozpis::getRozpisItem($oldId);

                    $newId = DBRozpis::addRozpis(
                        $data['r_trener'],
                        $data['r_kde'],
                        $data['r_datum'],
                        $data['r_visible'],
                        $data['r_lock']
                    );
                    foreach ($items as $item) {
                        DBRozpis::addRozpisItem(
                            $newId,
                            $item['ri_partner'],
                            $item['ri_od'],
                            $item['ri_do'],
                            $item['ri_lock']
                        );
                    }
                }
                $this->redirect('/admin/rozpis');
                break;
        }

        $data = array_map(
            function ($item) {
                $isTrainer = Permissions::check('rozpis', P_OWNED, $item['r_trener']);
                $isAdmin = Permissions::check('rozpis', P_ADMIN);
                return array(
                    'canEdit' => $isTrainer,
                    'fullName' => $item['u_jmeno'] . ' ' . $item['u_prijmeni'],
                    'datum' => formatDate($item['r_datum']),
                    'kde' => $item['r_kde'],
                    'checkBox' => (string) $this->checkbox('rozpis[]', $item['r_id'])
                                                ->readonly($isTrainer),
                    'visible' => (
                        $isAdmin ?
                        (string) $this->checkbox($item['r_id'], '1')
                                      ->defaultState($item['r_visible']) :
                        ('&nbsp;' . ($item['r_visible'] ? '&#10003;' : '&#10799;'))
                    ),
                    'links' => (
                        '<a href="/admin/rozpis/edit/' . $item['r_id'] . '">obecné</a>, ' .
                        '<a href="/admin/rozpis/detail/' . $item['r_id'] . '">tréninky</a>'
                    )
                );
            },
            DBRozpis::getRozpis(true)
        );
        
        $this->render(
            'files/View/Admin/Rozpis/Overview.inc',
            array(
                'showMenu' => !TISK,
                'data' => $data
            )
        );
        return;
    }
    
    public function add($request) {
        if (empty($_POST) || is_object($f = $this->_checkData())) {
            if (!empty($_POST))
                $this->redirect()->setMessage($f->getMessages());
            $this->render(
                'files/View/Admin/Rozpis/Form.inc',
                array(
                    'action' => $request->getAction(),
                    'isAdmin' => Permissions::check('rozpis', P_ADMIN)
                )
            );
            return;
        }
        Permissions::checkError('rozpis', P_OWNED, post('trener'));
        $datum = $this->date('datum')->getPost();
        $visible = (bool) post('visible');

        if (!Permissions::check('rozpis', P_ADMIN) && $visible) {
            $visible = false;
            $this->redirect()->setMessage('Nemáte dostatečná oprávnění ke zviditelnění příspěvku');
        }
        DBRozpis::addRozpis(post('trener'), post('kde'), (string) $datum, $visible, post('lock'));

        if ($visible) {
            $trener_data = DBUser::getUserData(post('trener'));
            $trener_name = $trener_data['u_jmeno'] . ' ' . $trener_data['u_prijmeni'];

            $n = new Novinky(User::getUserID());
            $n->rozpis()->add(
                '/member/rozpis',
                $datum->getDate(Date::FORMAT_SIMPLIFIED),
                $trener_name
            );
        }
        $this->redirect('/admin/rozpis', 'Rozpis přidán');
    }
    public function edit($request) {
        $id = $request->getId();
        if (!$id || !($data = DBRozpis::getSingleRozpis($id)))
            $this->redirect('/admin/rozpis', 'Rozpis s takovým ID neexistuje');
        Permissions::checkError('rozpis', P_OWNED, $data['r_trener']);

        if (empty($_POST) || is_object($f = $this->_checkData())) {
            if (empty($_POST)) {
                post('id', $id);
                post('trener', $data['r_trener']);
                post('kde', $data['r_kde']);
                post('datum', $data['r_datum']);
                post('visible', $data['r_visible']);
                post('lock', $data['r_lock']);
            } else {
                $this->redirect()->setMessage($f->getMessages());
            }
            $this->render(
                'files/View/Admin/Rozpis/Form.inc',
                array(
                    'action' => $request->getAction(),
                    'isAdmin' => Permissions::check('rozpis', P_ADMIN)
                )
            );
            return;
        }
        $datum = $this->date('datum')->getPost();

        $visible = (bool) post('visible');
        $visible_prev = $data['r_visible'];
        if (!Permissions::check('rozpis', P_ADMIN) && $visible && !$visible_prev) {
            $visible = $visible_prev;
            $this->redirect()->setMessage('Nemáte dostatečná oprávnění ke zviditelnění rozpisu');
        }
        DBRozpis::editRozpis(
            $id, post('trener'), post('kde'), (string) $datum,
            $visible, post('lock')
        );

        if ($visible) {
            if (!$visible_prev)
                $act = 'add';
            else
                $act = 'edit';
        } elseif (!$visible && $visible_prev && strcmp($datum, date('Y-m-d')) > 0) {
            $act = 'remove';
        }
        if (isset($act)) {
            $trener_data = DBUser::getUserData(post('trener'));
            $trener_name = $trener_data['u_jmeno'] . ' ' . $trener_data['u_prijmeni'];

            $n = new Novinky(User::getUserID());
            if ($act == 'remove') {
                $n->rozpis()->$act(
                    $datum->getDate(Date::FORMAT_SIMPLIFIED),
                    $trener_name
                );
            } else {
                $n->rozpis()->$act(
                    '/member/rozpis',
                    $datum->getDate(Date::FORMAT_SIMPLIFIED),
                    $trener_name
               );
            }
        }
        $this->redirect('/admin/rozpis', 'Rozpis úspěšně upraven');
    }

    private function _checkData() {
        $datum = $this->date('datum')->getPost();

        $f = new Form();
        $f->checkNumeric(post('trener'), 'Neplatný trenér', 'trener');
        $f->checkDate((string) $datum, 'Neplatný formát data', 'datum');

        return $f->isValid() ? true : $f;
    }
}
