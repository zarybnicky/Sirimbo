<?php
class Controller_Admin_Platby_Items extends Controller_Admin_Platby
{
    public function __construct()
    {
        parent::__construct();
        Permissions::checkError('platby', P_OWNED);
    }

    public function view($request)
    {
        $filter = [];
        if ($request->get('user') && is_numeric($request->get('user'))) {
            $filter['u_id'] = $request->get('user');
        }
        if ($request->get('category') && is_numeric($request->get('category'))) {
            $filter['pc_id'] = $request->get('category');
        } elseif (stripos($request->get('category'), 'group_') !== false) {
            $filter['pg_id'] = substr($request->get('category'), 6);
        }
        $date = $this->date('date')->range()->getPostRange($request);

        $data = DBPlatbyItem::get(true, $filter, ['pi_date DESC'], $date);

        $data = array_map(
            function ($item) {
                return [
                    'buttons' => $this->editLink('/admin/platby/items/edit/' . $item['pi_id'])
                        . '&nbsp;&nbsp;'
                        . $this->removeLink('/admin/platby/items/remove/' . $item['pi_id']),
                    'fullName' => $item['u_prijmeni'] . ', ' . $item['u_jmeno'],
                    'category' => $item['pc_name'],
                    'date' => (new Date($item['pi_date']))->getDate(Date::FORMAT_SIMPLE_SPACED),
                    'amount' => $item['pi_amount'] . ' Kč'
                ];
            },
            $data
        );

        $this->render('files/View/Admin/Platby/ItemsOverview.inc', [
            'header' => 'Správa plateb',
            'subheader' => 'Jednotlivé platby',
            'users' => DBUser::getUsers(),
            'categories' => $this->getCategories(),
            'data' => $data,
            'uri' => $request->getLiteralURI(),
            'user' => $request->get('user') ?: '',
            'category' => $request->get('category') ?: '',
            'date' => $request->get('date') ?: ''
        ]);
    }

    public function add($request)
    {
        if (!$request->post()) {
            $this->displayForm(0, $request);
            return;
        } elseif (!is_object($item = $this->getFromPost($request))) {
            $this->redirect()->warning($item);
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
        $this->redirect('/admin/platby/items');
    }

    public function edit($request)
    {
        if (!$id = $request->getId()) {
            $this->redirect()->warning('Platba s takovým ID neexistuje');
            $this->redirect('/admin/platby/items');
        }
        if (!$data = DBPlatbyItem::getSingle($id)) {
            $this->redirect()->warning('Platba s takovým ID neexistuje');
            $this->redirect('/admin/platby/items');
        }
        if (!$request->post()) {
            $request->post('date', $data['pi_date']);
            $request->post('amount', $data['pi_amount']);
            $request->post('variable', $data['pi_id_user']);
            $request->post('specific', $data['pi_id_category']);
            $request->post('prefix', $data['pi_prefix']);
            $this->displayForm($id, $request);
            return;
        } elseif (!is_object($item = $this->getFromPost($request, $id))) {
            $this->redirect()->warning($item);
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
        $this->redirect('/admin/platby/items');
    }

    public function remove($request)
    {
        if (!$request->getId()) {
            $this->redirect('/admin/platby/items');
        }
        $id = $request->getId();
        if ($request->post('action') == 'confirm') {
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
            $this->redirect('/admin/platby/items');
        }
        $item = DBPlatbyItem::getSingle($id, true);
        $this->render('files/View/Admin/RemovePrompt.inc', [
            'header' => 'Správa plateb',
            'prompt' => 'Opravdu chcete odstranit platbu:',
            'returnURI' => $request->getReferer() ?: '/admin/platby/items',
            'data' => [[
                'id' => $item['pi_id'],
                'text' => $item['u_jmeno'] . ' ' . $item['u_prijmeni']
                . ' - ' . $item['pc_name']
            ]]
        ]);
    }

    private function displayForm($id, $request)
    {
        $raw = [];
        if ($id &&
            ($item = DBPlatbyItem::getSingle($id)) &&
            ($data = DBPlatbyRaw::getSingle($item['pi_id_raw']))
        ) {
            $data = unserialize($data['pr_raw']);
            foreach ($data as $key => $value) {
                $raw[] = ['column' => $key, 'value' => $value];
            }
        }
        $users = $this->getUsers();
        $categories = $this->getCategories();
        $this->render('files/View/Admin/Platby/ItemsForm.inc', [
            'header' => 'Správa plateb',
            'subheader' => 'Jednotlivé platby',
            'action' => $request->getAction(),
            'returnURI' => $request->getReferer(),
            'id' => $id,
            'raw' => $raw,
            'users' => $users,
            'categories' => $categories,
            'date' => $request->post('date') ?: '',
            'amount' => $request->post('amount') ?: '',
            'variable' => $request->post('variable') ?: '',
            'specific' => $request->post('specific') ?: '',
            'prefix' => $request->post('prefix') ?: '',
            'uri' => $request->getLiteralURI()
        ]);
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
        foreach ($users as &$array) {
            $array = User::varSymbol($array['u_id']) . " - {$array['u_prijmeni']}, {$array['u_jmeno']}";
        }
        return $users;
    }
}