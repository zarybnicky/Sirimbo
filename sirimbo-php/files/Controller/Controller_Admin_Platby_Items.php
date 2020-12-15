<?php
class Controller_Admin_Platby_Items extends Controller_Admin_Platby
{
    public function view($request)
    {
        \Permissions::checkError('platby', P_OWNED);
        $filter = [];
        if ($_GET['user'] && is_numeric($_GET['user'])) {
            $filter['u_id'] = $_GET['user'];
        }
        if ($_GET['category'] && is_numeric($_GET['category'])) {
            $filter['pc_id'] = $_GET['category'];
        } elseif (stripos($_GET['category'], 'group_') !== false) {
            $filter['pg_id'] = substr($_GET['category'], 6);
        }
        $date = \DateHelper::getPostRange('date');

        $data = \DBPlatbyItem::get(true, $filter, ['pi_date DESC'], $date);

        $data = array_map(
            fn($item) => [
                'buttons' => new \EditLinkHelper('/admin/platby/items/edit/' . $item['pi_id'])
                . '&nbsp;&nbsp;'
                . new \RemoveLinkHelper('/admin/platby/items/remove/' . $item['pi_id']),
                'fullName' => $item['u_prijmeni'] . ', ' . $item['u_jmeno'],
                'category' => $item['pc_name'],
                'date' => (new \Date($item['pi_date']))->getHumanDate(),
                'amount' => $item['pi_amount'] . ' Kč'
            ],
            $data
        );

        new \RenderHelper('files/View/Admin/Platby/ItemsOverview.inc', [
            'header' => 'Správa plateb',
            'subheader' => 'Jednotlivé platby',
            'users' => \DBUser::getUsers(),
            'categories' => static::getCategories(),
            'data' => $data,
            'uri' => $request->getLiteralURI(),
            'user' => $_GET['user'] ?: '',
            'category' => $_GET['category'] ?: '',
            'date' => $_GET['date'] ?: ''
        ]);
    }

    public function add($request)
    {
        \Permissions::checkError('platby', P_OWNED);
        if (!$_POST) {
            return static::displayForm(0, $request);
        } elseif (!is_object($item = static::getFromPost())) {
            new \MessageHelper('warning', $item);
            return static::displayForm(0, $request);
        }
        \DBPlatbyItem::insert(
            $item->variable,
            $item->categoryId,
            null,
            $item->amount,
            $item->date,
            $item->prefix
        );
        new \RedirectHelper('/admin/platby/items');
    }

    public function edit($request)
    {
        \Permissions::checkError('platby', P_OWNED);
        if (!$id = $request->getId()) {
            new \MessageHelper('warning', 'Platba s takovým ID neexistuje');
            new \RedirectHelper('/admin/platby/items');
        }
        if (!$data = \DBPlatbyItem::getSingle($id)) {
            new \MessageHelper('warning', 'Platba s takovým ID neexistuje');
            new \RedirectHelper('/admin/platby/items');
        }
        if (!$_POST) {
            $_POST['date'] = $data['pi_date'];
            $_POST['amount'] = $data['pi_amount'];
            $_POST['variable'] = $data['pi_id_user'];
            $_POST['specific'] = $data['pi_id_category'];
            $_POST['prefix'] = $data['pi_prefix'];
            return static::displayForm($id, $request);
        } elseif (!is_object($item = static::getFromPost($id))) {
            new \MessageHelper('warning', $item);
            return static::displayForm($id, $request);
        }
        \DBPlatbyItem::update(
            $id,
            $item->variable,
            $item->categoryId,
            $item->amount,
            $item->date,
            $item->prefix
        );
        new \RedirectHelper('/admin/platby/items');
    }

    public function remove($request)
    {
        \Permissions::checkError('platby', P_OWNED);
        if (!$request->getId()) {
            new \RedirectHelper('/admin/platby/items');
        }
        $id = $request->getId();
        if ($_POST['action'] == 'confirm') {
            $item = \DBPlatbyItem::getSingle($id);
            $itemRaw = \DBPlatbyRaw::getSingle($item['pi_id_raw']);
            \DBPlatbyItem::remove($id);
            if ($item['pi_id_raw']) {
                \DBPlatbyRaw::update(
                    $item['pi_id_raw'],
                    $itemRaw['pr_raw'],
                    $itemRaw['pr_hash'],
                    '0',
                    '1'
                );
            }
            new \RedirectHelper('/admin/platby/items');
        }
        $item = \DBPlatbyItem::getSingle($id, true);
        new \RenderHelper('files/View/Admin/RemovePrompt.inc', [
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

    private static function displayForm($id, $request)
    {
        $raw = [];
        if ($id &&
            ($item = \DBPlatbyItem::getSingle($id)) &&
            ($data = \DBPlatbyRaw::getSingle($item['pi_id_raw']))
        ) {
            $data = unserialize($data['pr_raw']);
            foreach ($data as $key => $value) {
                $raw[] = ['column' => $key, 'value' => $value];
            }
        }
        $users = static::getUsers();
        $categories = static::getCategories();
        new \RenderHelper('files/View/Admin/Platby/ItemsForm.inc', [
            'header' => 'Správa plateb',
            'subheader' => 'Jednotlivé platby',
            'action' => $request->getAction(),
            'returnURI' => $request->getReferer(),
            'id' => $id,
            'raw' => $raw,
            'users' => $users,
            'categories' => $categories,
            'date' => $_POST['date'] ?: '',
            'amount' => $_POST['amount'] ?: '',
            'variable' => $_POST['variable'] ?: '',
            'specific' => $_POST['specific'] ?: '',
            'prefix' => $_POST['prefix'] ?: '',
            'uri' => $request->getLiteralURI()
        ]);
    }

    private static function getCategories()
    {
        $out = static::getCategoryLookup(false, false, true);
        foreach ($out as $key => &$array) {
            if (strpos($key, 'group_') !== false) {
                $array = "{$array['pg_name']}:";
            } else {
                $array = "{$array['pc_symbol']} - {$array['pc_name']}";
            }
        }
        return $out;
    }

    private static function getUsers()
    {
        $users = static::getUserLookup(true);
        foreach ($users as &$array) {
            $array = User::varSymbol($array['u_id']) . " - {$array['u_prijmeni']}, {$array['u_jmeno']}";
        }
        return $users;
    }
}
