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

        $date = new \DateHelper('date', $_GET['date'] ?? null);
        \Render::twig('Admin/PlatbyItems.twig', [
            'users' => [['u_id' => 'all', 'u_prijmeni' => '---', 'u_jmeno' => '']] + \DBUser::getUsers(),
            'categories' => ['all' => '---'] + static::getCategories(),
            'data' => \DBPlatbyItem::get(
                true,
                $filter,
                ['from' => $date->getFromDate(), 'to' => $date->getToDate()]
            ),
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
            \Message::warning($item);
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
        \Redirect::to('/admin/platby/items');
    }

    public static function edit($id)
    {
        \Permissions::checkError('platby', P_OWNED);
        if (!$data = \DBPlatbyItem::getSingle($id)) {
            \Message::warning('Platba s takovým ID neexistuje');
            \Redirect::to('/admin/platby/items');
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
            \Message::warning('Platba s takovým ID neexistuje');
            \Redirect::to('/admin/platby/items');
        }
        if (!is_object($item = Platby::getFromPost($id))) {
            \Message::warning($item);
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
        \Redirect::to('/admin/platby/items');
    }

    public static function remove($id)
    {
        \Permissions::checkError('platby', P_OWNED);
        $item = \DBPlatbyItem::getSingle($id, true);
        \Render::twig('RemovePrompt.twig', [
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
                stream_get_contents($itemRaw['pr_raw']),
                $itemRaw['pr_hash'],
                '0',
                '1'
            );
        }
        \Redirect::to('/admin/platby/items');
    }

    private static function displayForm($id, $action)
    {
        $raw = [];
        if ($id &&
            ($item = \DBPlatbyItem::getSingle($id)) && $item['pi_id_raw'] &&
            ($data = \DBPlatbyRaw::getSingle($item['pi_id_raw']))
        ) {
            foreach (unserialize(stream_get_contents($data['pr_raw'])) as $key => $value) {
                $raw[] = ['column' => $key, 'value' => $value];
            }
        }
        \Render::twig('Admin/PlatbyItemsForm.twig', [
            'action' => $action,
            'returnURI' => $_SERVER['HTTP_REFERER'],
            'id' => $id,
            'raw' => $raw,
            'users' => array_for(Platby::getUserLookup(true), function ($x) {
                $u = \User::fromArray($x);
                return "{$u->getVarSymbol()} - {$x['u_prijmeni']}, {$x['u_jmeno']}";
            }),
            'categories' => static::getCategories(),
            'date' => $_POST['date'] ?? '',
            'amount' => $_POST['amount'] ?? '',
            'variable' => $_POST['variable'] ?? '',
            'specific' => $_POST['specific'] ?? '',
            'prefix' => $_POST['prefix'] ?? '',
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
}