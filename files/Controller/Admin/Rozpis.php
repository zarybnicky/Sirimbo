<?php
require_once 'files/Controller/Admin.php';
class Controller_Admin_Rozpis extends Controller_Admin
{
    public function __construct() {
        Permissions::checkError('rozpis', P_OWNED);
    }
    public function view($request) {
        switch($request->post('action')) {
        case 'save':
            $items = DBRozpis::getRozpis();
            foreach ($items as $item) {
                $id = $item['r_id'];
                if ((bool) $request->post($id) == (bool) $item['r_visible']
                    || !Permissions::check('rozpis', P_OWNED, $item['r_trener'])
                ) {
                    continue;
                }
                if (!Permissions::check('rozpis', P_ADMIN) &&
                    $request->post($id) &&
                    !$item['r_visible']
                ) {
                    $this->redirect()->setMessage('Nemáte dostatečná oprávnění ke zviditelnění rozpisu');
                } else {
                    DBRozpis::editRozpis(
                        $id,
                        $item['r_trener'],
                        $item['r_kde'],
                        $item['r_datum'],
                        $request->post($id) ? '1' : '0',
                        $item['r_lock'] ? '1' : '0'
                    );
                }
            }
            break;

        case 'remove':
            if (!is_array($request->post('rozpis'))) {
                $this->redirect('/admin/rozpis');
            }
            foreach ($request->post('rozpis') as $item) {
                $trener = DBRozpis::getRozpisTrener($item);
                $data = DBRozpis::getSingleRozpis($item);

                if (Permissions::check('rozpis', P_OWNED, $trener['u_id'])) {
                    DBRozpis::removeRozpis($item);
                } else {
                    $error = true;
                }
            }
            if (isset($error) && $error) {
                throw new AuthorizationException("Máte nedostatečnou autorizaci pro tuto akci!");
            }

            $this->redirect('/admin/rozpis', 'Rozpisy odebrány');

        case 'duplicate':
            foreach ($request->post('rozpis') as $oldId) {
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
                        $this->checkbox($item['r_id'], '1')
                             ->set($item['r_visible'])
                             ->render() :
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

    public function add($request)
    {
        if (!$request->post()) {
            $this->displayForm($request);
            return;
        }

        $form = $this->checkData($request);
        if (is_object($form)) {
            $this->redirect()->setMessage($f->getMessages());
            $this->displayForm($request);
            return;
        }

        Permissions::checkError('rozpis', P_OWNED, $request->post('trener'));
        $datum = $this->date('datum')->getPost($request);
        $visible = (bool) $request->post('visible');

        if (!Permissions::check('rozpis', P_ADMIN) && $visible) {
            $visible = false;
            $this->redirect()->setMessage('Nemáte dostatečná oprávnění ke zviditelnění příspěvku');
        }
        DBRozpis::addRozpis(
            $request->post('trener'),
            $request->post('kde'),
            (string) $datum,
            $visible ? '1' : '0',
            $request->post('lock') ? '1' : '0'
        );
        $this->redirect('/admin/rozpis', 'Rozpis přidán');
    }

    public function edit($request)
    {
        $id = $request->getId();
        if (!$id || !($data = DBRozpis::getSingleRozpis($id))) {
            $this->redirect('/admin/rozpis', 'Rozpis s takovým ID neexistuje');
        }
        Permissions::checkError('rozpis', P_OWNED, $data['r_trener']);

        if (!$request->post()) {
            $this->displayForm($request, $data);
        }

        $form = $this->checkData($request);
        if (is_object($form)) {
            $this->redirect()->setMessage($form->getMessages());
            $this->displayForm($request, $data);
            return;
        }

        $datum = $this->date('datum')->getPost($request);

        $visible = (bool) $request->post('visible');
        $visible_prev = $data['r_visible'];
        if (!Permissions::check('rozpis', P_ADMIN) && $visible && !$visible_prev) {
            $visible = $visible_prev;
            $this->redirect()->setMessage('Nemáte dostatečná oprávnění ke zviditelnění rozpisu');
        }
        DBRozpis::editRozpis(
            $id,
            $request->post('trener'),
            $request->post('kde'),
            (string) $datum,
            $visible,
            $request->post('lock') ? '1' : '0'
        );

        $this->redirect('/admin/rozpis', 'Rozpis úspěšně upraven');
    }

    protected function displayForm($request, $data = null)
    {
        $isAdmin = Permissions::check('rozpis', P_ADMIN);
        if ($isAdmin) {
            $treneri = DBUser::getUsersByPermission('rozpis', P_OWNED);
        } else {
            $treneri = array(DBUser::getUserData(User::getUserID()));
        }

        $this->render(
            'files/View/Admin/Rozpis/Form.inc',
            array(
                'action' => $request->getAction(),
                'isAdmin' => $isAdmin,
                'treneri' => $treneri,
                'trener' => $request->post('trener') ?: $data ? $data['r_trener'] : '',
                'kde' => $request->post('kde') ?: $data ? $data['r_kde'] : '',
                'datum' => $request->post('datum') ?: $data ? $data['r_datum'] : '',
                'visible' => $request->post('visible') ?: $data ? $data['r_visible'] : '',
                'lock' => $request->post('lock') ?: $data ? $data['r_lock'] : ''
            )
        );
    }

    private function checkData($request)
    {
        $datum = $this->date('datum')->getPost($request);

        $f = new Form();
        $f->checkNumeric($request->post('trener'), 'Neplatný trenér', 'trener');
        $f->checkDate((string) $datum, 'Neplatný formát data', 'datum');

        return $f->isValid() ? true : $f;
    }
}
