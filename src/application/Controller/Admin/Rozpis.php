<?php
namespace TKOlomouc\Controller\Admin;

use TKOlomouc\Controller\Admin;
use TKOlomouc\Utility\Permissions;
use TKOlomouc\Utility\Novinky;
use TKOlomouc\Utility\User;
use TKOlomouc\Utility\Request;
use TKOlomouc\Utility\Form;
use TKOlomouc\Model\DBRozpis;
use TKOlomouc\Model\DBUser;
use TKOlomouc\View\Helper\Date;
use TKOlomouc\View\Exception\AuthorizationException;
use TKOlomouc\Type\DateFormat;

class Rozpis extends Admin
{
    public function __construct()
    {
        Permissions::checkError('rozpis', P_OWNED);
    }

    public function view($id = null)
    {
        switch(post('action')) {
            case 'save':
                $this->processSave();
                break;
            case 'edit':
                $rozpis = post('rozpis');
                if ($rozpis[0])
                    $this->redirect('/admin/rozpis/edit/' . $rozpis[0]);
                break;
            case 'edit_detail':
                $rozpis = post('rozpis');
                if ($rozpis[0])
                    $this->redirect('/admin/rozpis/detail/' . $rozpis[0]);
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
                                (new Date($data['r_datum']))->getDate(DateFormat::FORMAT_SIMPLIFIED),
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
        }
        $data = DBRozpis::getRozpis();
        foreach ($data as &$row) {
            $new_data = array(
                'canEdit' => Permissions::check('rozpis', P_OWNED, $row['r_trener']),
                'fullName' => $row['u_jmeno'] . ' ' . $row['u_prijmeni'],
                'datum' => formatDate($row['r_datum']),
                'kde' => $row['r_kde']
            );
            if ($new_data['canEdit'])
                $new_data['checkBox'] = '<input type="checkbox" name="rozpis[]" value="' . $row['r_id'] . '" />';
            else
                $new_data['checkBox'] = '<input type="checkbox" name="rozpis[]" value="" disabled="d" />';
            if (Permissions::check('rozpis', P_ADMIN))
                $new_data['visible'] = getCheckbox($row['r_id'], '1', $row['r_visible']);
            else
                $new_data['visible'] = '&nbsp;' . ($row['r_visible'] ? '&#10003;' : '&#10799;');
            $row = $new_data;
        }
        $this->render(
            'src/application/View/Admin/Rozpis/Overview.inc',
            array(
                'showMenu' => !TISK,
                'data' => $data
            )
        );
        return;
    }
    function add($id = null) {
        if (empty($_POST) || is_object($f = $this->checkData())) {
            if (!empty($_POST))
                $this->redirect()->setMessage($f->getMessages());
            $this->render(
                'src/application/View/Admin/Rozpis/Form.inc',
                array(
                    'action' => Request::getAction(),
                    'isAdmin' => Permissions::check('rozpis', P_ADMIN),
                    'userId' => User::getUserID(),
                    'tutors' => DBUser::getUsersByPermission('rozpis', P_OWNED)
                )
            );
            return;
        }
        Permissions::checkError('rozpis', P_OWNED, post('trener'));
        $datum = (new Date('datum'))->getPost();
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
                $datum->getDate(DateFormat::FORMAT_SIMPLIFIED),
                $trener_name
            );
        }
        $this->redirect('/admin/rozpis', 'Rozpis přidán');
    }
    function edit($id = null) {
        if (!$id || !($data = DBRozpis::getSingleRozpis($id)))
            $this->redirect('/admin/rozpis', 'Rozpis s takovým ID neexistuje');
        Permissions::checkError('rozpis', P_OWNED, $data['r_trener']);

        if (empty($_POST) || is_object($this->checkData())) {
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
                'src/application/View/Admin/Rozpis/Form.inc',
                array(
                    'action' => Request::getAction(),
                    'isAdmin' => Permissions::check('rozpis', P_ADMIN),
                    'userId' => User::getUserID(),
                    'tutors' => DBUser::getUsersByPermission('rozpis', P_OWNED)
                )
            );
            return;
        }
        $datum = (new Date('datum'))->getPost();

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
                    $datum->getDate(DateFormat::FORMAT_SIMPLIFIED),
                    $trener_name
                );
            } else {
                $n->rozpis()->$act(
                    '/member/rozpis',
                    $datum->getDate(DateFormat::FORMAT_SIMPLIFIED),
                    $trener_name
               );
            }
        }
        $this->redirect('/admin/rozpis', 'Rozpis úspěšně upraven');
    }

    private function processSave()
    {
        $items = DBRozpis::getRozpis();
        foreach ($items as $item) {
            $id = $item['r_id'];
            if ((bool) post($id) === (bool) $item['r_visible']) {
                continue;
            }
            if(!Permissions::check('rozpis', P_OWNED, $item['r_trener'])) {
                continue;
            }
            if (!Permissions::check('rozpis', P_ADMIN) && post($id) && !$item['r_visible']) {
                $this->redirect()->setMessage('Nemáte dostatečná oprávnění ke zviditelnění rozpisu');
                continue;
            }

            DBRozpis::editRozpis(
                $id, $item['r_trener'], $item['r_kde'],
                $item['r_datum'], post($id) ? '1' : '0', $item['r_lock'] ? '1' : '0'
            );

            $n = new Novinky(User::getUserID());

            if (!post($id)) {
                $n->rozpis()->remove(
                    (new Date($item['r_datum']))->getDate(DateFormat::FORMAT_SIMPLIFIED),
                    $item['u_jmeno'] . ' ' . $item['u_prijmeni']
                );
            } else {
                $n->rozpis()->add(
                    '/member/rozpis',
                    (new Date($item['r_datum']))->getDate(DateFormat::FORMAT_SIMPLIFIED),
                    $item['u_jmeno'] . ' ' . $item['u_prijmeni']
                );
            }
        }
    }

    private function checkData() {
        $datum = (new Date('datum'))->getPost();

        $f = new Form();
        $f->checkNumeric(post('trener'), 'Neplatný trenér', 'trener');
        $f->checkDate((string) $datum, 'Neplatný formát data', 'datum');

        return $f->isValid() ? true : $f;
    }
}
