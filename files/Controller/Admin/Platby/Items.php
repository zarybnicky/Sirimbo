<?php
require_once 'files/Controller/Admin/Platby.php';
class Controller_Admin_Platby_Items extends Controller_Admin_Platby
{
    public function __construct()
    {
        Permissions::checkError('platby', P_OWNED);
    }

    public function view($request)
    {
        switch($request->post('action')) {
        case 'edit':
            $users = $request->post('data');
            if ($users[0]) {
                $this->redirect('/admin/platby/items/edit/' . $users[0]);
            }
            break;
        case 'remove':
            if (is_array($request->post('data'))) {
                $this->redirect(
                    '/admin/platby/items/remove?'
                    . http_build_query(array('u' => $request->post('data')))
                );
            }
            break;
        }
        $data = $this->getData($request);
        $this->render(
            'files/View/Admin/Platby/ItemsOverview.inc',
            array(
                'users' => DBUser::getUsers(),
                'categories' => $this->getCategories(),
                'data' => $data,
                'uri' => $request->getLiteralURI(),
                'user' => $request->get('user') ?: '',
                'category' => $request->get('category') ?: '',
                'date' => $request->get('date') ?: ''
            )
        );
    }
    public function add($request)
    {
        if (!$request->post()) {
            $this->displayForm(0, $request);
            return;
        } elseif (!is_object($item = $this->getFromPost())) {
            $this->redirect()->setMessage($item);
            $this->displayForm(0, $request);
            return;
        }
        DBPlatbyItem::insert(
            $item->variable,
            $item->categoryId,
            null,
            $item->amount,
            $item->date,
            $item->prefix
        );
        $this->redirect('/admin/platby/items', 'Platba úspěšně přidána');
    }
    public function edit($request)
    {
        $id = $request->getId();
        if (!$id || !($data = DBPlatbyItem::getSingle($id))) {
            $this->redirect('/admin/platby/items', 'Platba s takovým ID neexistuje');
        }
        if (!$request->post()) {
            $request->post('date', $data['pi_date']);
            $request->post('amount', $data['pi_amount']);
            $request->post('variable', $data['pi_id_user']);
            $request->post('specific', $data['pi_id_category']);
            $request->post('prefix', $data['pi_prefix']);
            $this->displayForm($id, $request);
            return;
        } elseif (!is_object($item = $this->getFromPost($id))) {
            $this->redirect()->setMessage($item);
            $this->displayForm($id, $request);
            return;
        }
        DBPlatbyItem::update(
            $id,
            $item->variable,
            $item->categoryId,
            $item->amount,
            $item->date,
            $item->prefix
        );
        $this->redirect('/admin/platby/items', 'Platba úspěšně upravena');
    }

    public function remove($request)
    {
        if (!is_array($request->post('data')) && !is_array($request->get('u'))) {
            $this->redirect('/admin/platby/items');
        }
        if ($request->post() && $request->post('action') == 'confirm') {
            foreach ($request->post('data') as $id) {
                $item = DBPlatbyItem::getSingle($id);
                $itemRaw = DBPlatbyRaw::getSingle($item['pi_id_raw']);

                DBPlatbyItem::remove($id);
                if ($item['pi_id_raw']) {
                    DBPlatbyRaw::update(
                        $item['pi_id_raw'],
                        $itemRaw['pr_raw'],
                        $itemRaw['pr_hash'],
                        '0',
                        '1'
                    );
                }
            }
            $this->redirect('/admin/platby/items', 'Platby odebrány');
        }
        $data = array();
        foreach ($request->get('u') as $id) {
            $item = DBPlatbyItem::getSingle($id, true);
            $data[] = array(
                'id' => $item['pi_id'],
                'text' => $item['u_jmeno'] . ' ' . $item['u_prijmeni']
                    . ' - ' . $item['pc_name']
            );
        }
        $this->render(
            'files/View/Admin/RemovePrompt.inc',
            array(
                'header' => 'Správa plateb',
                'prompt' => 'Opravdu chcete odstranit platby:',
                'returnURI' => $request->getReferer(),
                'data' => $data
            )
        );
    }
    private function displayForm($request, $id)
    {
        $raw = array();
        if (
            $id &&
            ($item = DBPlatbyItem::getSingle($id)) &&
            ($data = DBPlatbyRaw::getSingle($item['pi_id_raw']))
        ) {
            $data = unserialize($data['pr_raw']);
            foreach ($data as $key => $value) {
                $raw[] = array(
                    'column' => $key,
                    'value' => $value
                );
            }
        }
        $users = $this->getUsers();
        $categories = $this->getCategories();
        $this->render(
            'files/View/Admin/Platby/ItemsForm.inc',
            array(
                'action' => $request->getAction(),
                'referer' => $request->getReferer(),
                'id' => $id,
                'raw' => $raw,
                'users' => $users,
                'categories' => $categories,
                'date' => $request->post('date'),
                'amount' => $request->post('amount'),
                'variable' => $request->post('variable'),
                'specific' => $request->post('specific'),
                'prefix' => $request->post('prefix'),
                'uri' => $request->getLiteralURI()
            )
        );
    }
    private function getCategories()
    {
        $out = $this->getCategoryLookup(false, false, true);
        foreach ($out as $key => &$array) {
            if (strpos($key, 'group_') !== false) {
                $array = "{$array['pg_name']}:";
            } else {
                $array = "{$array['pc_symbol']} - {$array['pc_name']}";
            }
        }
        return $out;
    }
    private function getUsers()
    {
        $users = $this->getUserLookup(true);
        foreach ($users as $key => &$array) {
            $array = User::varSymbol($array['u_id']) . " - {$array['u_prijmeni']}, {$array['u_jmeno']}";
        }
        return $users;
    }
    private function getData($request)
    {
        $filter = array();
        if ($request->get('user') && is_numeric($request->get('user'))) {
            $filter['u_id'] = $request->get('user');
        }
        if ($request->get('category') && is_numeric($request->get('category'))) {
            $filter['pc_id'] = $request->get('category');
        } elseif (stripos($request->get('category'), 'group_') !== false) {
            $filter['pg_id'] = substr($request->get('category'), 6);
        }
        $dateHelper = $this->date('date')->range()->textBox();
        $date = $dateHelper->getPostRange($request);

        $data = DBPlatbyItem::get(true, $filter, array('pi_date DESC'), $date);

        return array_map(
            function ($item) {
                return array(
                    'checkBox' => $this->checkbox('data[]', $item['pi_id'])->render(),
                    'fullName' => $item['u_prijmeni'] . ', ' . $item['u_jmeno'],
                    'category' => $item['pc_name'],
                    'date' => (new Date($item['pi_date']))->getDate(Date::FORMAT_SIMPLE_SPACED),
                    'amount' => $item['pi_amount'] . 'Kč'
                );
            }
        );
    }
}
