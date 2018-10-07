<?php
require_once 'files/Controller/Admin.php';
class Controller_Admin_Rozpis extends Controller_Admin
{
    public function __construct()
    {
        Permissions::checkError('rozpis', P_OWNED);
    }

    public function view($request)
    {
        $data = Permissions::check('rozpis', P_ADMIN)
            ? DBRozpis::getRozpis(true)
            : DBRozpis::getRozpisyByTrener(User::getUserID(), true);

        if ($request->post('action') == 'save') {
            foreach ($data as $item) {
                $id = $item['r_id'];
                if ((bool) $request->post($id) == (bool) $item['r_visible']) {
                    continue;
                }
                DBRozpis::editRozpis(
                    $id,
                    $item['r_trener'],
                    $item['r_kde'],
                    $item['r_datum'],
                    $request->post($id) ? '1' : '0',
                    $item['r_lock'] ? '1' : '0'
                );
            }
            $this->redirect('/admin/rozpis');
        }

        $data = array_map(
            function ($item) {
                return [
                    'fullName' => $item['u_jmeno'] . ' ' . $item['u_prijmeni'],
                    'datum' => formatDate($item['r_datum']),
                    'kde' => $item['r_kde'],
                    'visible' =>
                        (string) $this->checkbox($item['r_id'], '1')->set($item['r_visible']),
                    'buttons' => $this->duplicateLink('/admin/rozpis/duplicate/' . $item['r_id'])
                        . '&nbsp;' . $this->removeLink('/admin/rozpis/remove/' . $item['r_id']),
                    'links' => (
                        '<a href="/admin/rozpis/edit/' . $item['r_id'] . '">obecné</a>, ' .
                        '<a href="/admin/rozpis/detail/' . $item['r_id'] . '">tréninky</a>'
                    )
                ];
            },
            $data
        );

        $this->render('files/View/Admin/Rozpis/Overview.inc', [
            'header' => 'Správa rozpisů',
            'showMenu' => !TISK,
            'data' => $data
        ]);
    }

    public function add($request)
    {
        if (!$request->post()) {
            $this->displayForm($request);
            return;
        }

        $form = $this->checkData($request);
        if (is_object($form)) {
            $this->redirect()->setMessage($form->getMessages());
            $this->displayForm($request);
            return;
        }

        Permissions::checkError('rozpis', P_OWNED, $request->post('trener'));
        $datum = $this->date('datum')->getPost($request);

        DBRozpis::addRozpis(
            $request->post('trener'),
            $request->post('kde'),
            (string) $datum,
            $request->post('visible') ? '1' : '0',
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

        DBRozpis::editRozpis(
            $id,
            $request->post('trener'),
            $request->post('kde'),
            (string) $datum,
            $request->post('visible') ? '1' : '0',
            $request->post('lock') ? '1' : '0'
        );

        $this->redirect('/admin/rozpis', 'Rozpis úspěšně upraven');
    }

    public function duplicate($request)
    {
        $oldId = $request->getId();
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
        $this->redirect('/admin/rozpis');
    }

    public function remove($request)
    {
        $id = $request->getId();
        $trener = DBRozpis::getRozpisTrener($id);

        if (!Permissions::check('rozpis', P_OWNED, $trener['u_id'])) {
            throw new AuthorizationException("Máte nedostatečnou autorizaci pro tuto akci!");
        }
        DBRozpis::removeRozpis($id);
        $this->redirect('/admin/rozpis');
    }

    protected function displayForm($request, $data = null)
    {
        $isAdmin = Permissions::check('rozpis', P_ADMIN);
        $treneri = $isAdmin
                 ? DBUser::getUsersByPermission('rozpis', P_OWNED)
                 : [DBUser::getUserData(User::getUserID())];

        $this->render('files/View/Admin/Rozpis/Form.inc', [
            'header' => 'Správa rozpisů',
            'subheader' => ($this->action == 'add' ? 'Přidat' : 'Upravit') . ' rozpis',
            'action' => $request->getAction(),
            'isAdmin' => $isAdmin,
            'treneri' => $treneri,
            'trener' => $request->post('trener') ?: ($data ? $data['r_trener'] : ''),
            'kde' => $request->post('kde') ?: ($data ? $data['r_kde'] : ''),
            'datum' => $request->post('datum') ?: ($data ? $data['r_datum'] : ''),
            'visible' => $request->post('visible') ?: ($data ? $data['r_visible'] : ''),
            'lock' => $request->post('lock') ?: ($data ? $data['r_lock'] : '')
        ]);
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
