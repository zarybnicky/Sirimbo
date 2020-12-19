<?php
namespace Olymp\Controller\Admin;

class PlatbyItems
{
    public static function list()
    {
        \Permissions::checkError('platby', P_OWNED);
        $filter = [];
        if (is_numeric($_GET['user'] ?? null)) {
            $filter['u_id'] = $_GET['user'];
        }
        if (is_numeric($_GET['category'] ?? null)) {
            $filter['pc_id'] = $_GET['category'];
        } elseif (stripos($_GET['category'] ?? '', 'group_') !== false) {
            $filter['pg_id'] = substr($_GET['category'], 6);
        }

        $data = array_map(
            fn($item) => [
                'buttons' => \Buttons::platbyItem($item['pi_id']),
                'fullName' => $item['u_prijmeni'] . ', ' . $item['u_jmeno'],
                'category' => $item['pc_name'],
                'date' => (new \Date($item['pi_date']))->getHumanDate(),
                'amount' => $item['pi_amount'] . ' Kč'
            ],
            \DBPlatbyItem::get(true, $filter, ['pi_date DESC'], \DateHelper::getPostRange('date')),
        );

        new \RenderHelper('files/View/Admin/Platby/ItemsOverview.inc', [
            'header' => 'Správa plateb',
            'subheader' => 'Jednotlivé platby',
            'users' => \DBUser::getUsers(),
            'categories' => static::getCategories(),
            'data' => $data,
            'uri' => trim(explode('?', $_SERVER['REQUEST_URI'])[0], '/'),
            'user' => $_GET['user'] ?? '',
            'category' => $_GET['category'] ?? '',
            'date' => $_GET['date'] ?? ''
        ]);
    }

    public static function add()
    {
        \Permissions::checkError('platby', P_OWNED);
        return static::displayForm(0, 'add');
    }

    public static function addPost()
    {
        if (!is_object($item = Platby::getFromPost())) {
            new \MessageHelper('warning', $item);
            return static::add();
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

    public static function edit($id)
    {
        \Permissions::checkError('platby', P_OWNED);
        if (!$data = \DBPlatbyItem::getSingle($id)) {
            new \MessageHelper('warning', 'Platba s takovým ID neexistuje');
            new \RedirectHelper('/admin/platby/items');
        }
        $_POST['date'] = $data['pi_date'];
        $_POST['amount'] = $data['pi_amount'];
        $_POST['variable'] = $data['pi_id_user'];
        $_POST['specific'] = $data['pi_id_category'];
        $_POST['prefix'] = $data['pi_prefix'];
        return static::displayForm($id, 'edit');
    }

    public static function editPost($id)
    {
        \Permissions::checkError('platby', P_OWNED);
        if (!\DBPlatbyItem::getSingle($id)) {
            new \MessageHelper('warning', 'Platba s takovým ID neexistuje');
            new \RedirectHelper('/admin/platby/items');
        }
        if (!is_object($item = Platby::getFromPost($id))) {
            new \MessageHelper('warning', $item);
            return static::displayForm($id, 'edit');
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

    public static function remove($id)
    {
        \Permissions::checkError('platby', P_OWNED);
        $item = \DBPlatbyItem::getSingle($id, true);
        new \RenderHelper('files/View/Admin/RemovePrompt.inc', [
            'header' => 'Správa plateb',
            'prompt' => 'Opravdu chcete odstranit platbu:',
            'returnURI' => $_SERVER['HTTP_REFERER'] ?? '/admin/platby/items',
            'data' => [[
                'id' => $item['pi_id'],
                'text' => $item['u_jmeno'] . ' ' . $item['u_prijmeni']
                . ' - ' . $item['pc_name']
            ]]
        ]);
    }

    public static function removePost($id)
    {
        \Permissions::checkError('platby', P_OWNED);
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

    private static function displayForm($id, $action)
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
        new \RenderHelper('files/View/Admin/Platby/ItemsForm.inc', [
            'header' => 'Správa plateb',
            'subheader' => 'Jednotlivé platby',
            'action' => $action,
            'returnURI' => $_SERVER['HTTP_REFERER'],
            'id' => $id,
            'raw' => $raw,
            'users' => static::getUsers(),
            'categories' => static::getCategories(),
            'date' => $_POST['date'] ?? '',
            'amount' => $_POST['amount'] ?? '',
            'variable' => $_POST['variable'] ?? '',
            'specific' => $_POST['specific'] ?? '',
            'prefix' => $_POST['prefix'] ?? '',
            'uri' => trim(explode('?', $_SERVER['REQUEST_URI'])[0], '/')
        ]);
    }

    private static function getCategories()
    {
        $out = Platby::getCategoryLookup(false, false, true);
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
        $users = Platby::getUserLookup(true);
        foreach ($users as &$array) {
            $array = \User::varSymbol($array['u_id']) . " - {$array['u_prijmeni']}, {$array['u_jmeno']}";
        }
        return $users;
    }
}
