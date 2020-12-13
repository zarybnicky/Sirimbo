<?php
class Controller_Admin_Akce_Detail extends Controller_Abstract
{
    public function __construct($request)
    {
        parent::__construct($request);
        Permissions::checkError('akce', P_OWNED);
    }

    public function view($request)
    {
        if (!$id = $request->getId()) {
            $this->redirect()->warning('Akce s takovým ID neexistuje');
            $this->redirect('/admin/akce');
        }
        if (!$akce = DBAkce::getSingleAkce($id)) {
            $this->redirect()->warning('Akce s takovým ID neexistuje');
            $this->redirect('/admin/akce');
        }

        if ($request->post()) {
            if ($request->post("remove") > 0) {
                DBAkce::removeAkceItem($request->post("remove"));
            }

            foreach (DBAkce::getAkceItems($id) as $item) {
                $user = $request->post($item["ai_id"] . '-user');

                if (!$user) {
                    DBAkce::removeAkceItem($item['ai_id']);
                } elseif ($user != $item["ai_user"]) {
                    $data = DBUser::getUserData($user);
                    list($year) = explode('-', $data['u_narozeni']);
                    DBAkce::editAkceItem($item["ai_id"], $user, $year);
                }
            }

            if (is_numeric($request->post("add-user")) && $request->post('add-user') > 0) {
                $user = $request->post("add-user");
                $data = DBUser::getUserData($user);
                list($year) = explode('-', $data['u_narozeni']);

                DBAkce::addAkceItem($id, $user, $year);
                $request->post('add-user', 0);
            }
            $this->redirect('/admin/akce/detail/' . $id);
        }

        $data = [
            'id' => $akce['a_id'],
            'jmeno' => $akce['a_jmeno'],
            'kde' => $akce['a_kde'],
            'datum' => formatDate($akce['a_od'])
                . (($akce['a_od'] != $akce['a_do'])
                ? ' - ' . formatDate($akce['a_do'])
                : ''),
            'kapacita' => $akce['a_kapacita'],
            'volno' => $akce['a_kapacita'] - count(DBAkce::getAkceItems($id)),
            'showForm' => Permissions::check('akce', P_MEMBER)
                && !$akce['a_lock'],
            'canEdit' => Permissions::check('akce', P_OWNED),
            'info' => nl2br($akce['a_info'])
        ];

        $userSelect = new UserSelectHelper(DBUser::getActiveUsers());
        $items = array_map(
            function ($item) use ($userSelect) {
                return [
                    'name' => $userSelect->name($item['ai_id'] . '-user')->set($item['ai_user']),
                    'removeButton' => $this->submit('Odstranit')->data('remove', $item['ai_id'])
                ];
            },
            DBAkce::getAkceItems($id)
        );
        $items[] = [
            'name' => $userSelect->name('add-user')->set(0),
            'removeButton' => $this->submit('Přidat')->data('add', 'add')
        ];

        $this->render('files/View/Admin/Akce/Detail.inc', [
            'header' => 'Správa akcí',
            'data' => $data,
            'items' => $items
        ]);
    }
}
