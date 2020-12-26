<?php
namespace Olymp\Controller\Admin;

class PlatbyGroup
{
    public static function list()
    {
        \Permissions::checkError('platby', P_OWNED);
        \Render::page('files/View/Admin/Platby/StructureGroupOverview.inc', [
            'header' => 'Správa plateb',
            'subheader' => 'Kategorie plateb',
            'data' => array_map(
                fn($item) => [
                    'name' => $item['pg_name'],
                    'type' => $item['pg_type'] ? 'Členské příspěvky' : 'Běžné platby',
                    'buttons' => \Buttons::platbyGroup($item['pg_id']),
                ],
                \DBPlatbyGroup::getGroups()
            ),
        ]);
    }

    public static function add()
    {
        \Permissions::checkError('platby', P_OWNED);
        $_POST['base'] = 1;
        return static::displayForm('add', 0);
    }

    public static function addPost()
    {
        \Permissions::checkError('platby', P_OWNED);
        $form = static::checkData();
        if (!$form->isValid()) {
            \Message::warning($form->getMessages());
            return static::displayForm('add', 0);
        }
        \DBPlatbyGroup::insert(
            $_POST['type'],
            $_POST['name'],
            $_POST['description'],
            $_POST['base']
        );
        $insertId = \DBPlatbyGroup::getInsertId();

        foreach ($_POST['category'] ?? [] as $item) {
            \DBPlatbyGroup::addChild($insertId, $item);
        }
        foreach ($_POST['skupiny'] ?? [] as $item) {
            \DBSkupiny::addChild($item, $insertId);
        }
        \Redirect::to($_POST['returnURI'] ?? '/admin/platby/structure/group');
    }

    public static function edit($id)
    {
        \Permissions::checkError('platby', P_OWNED);
        if (!$data = \DBPlatbyGroup::getSingle($id)) {
            \Message::warning('Kategorie s takovým ID neexistuje');
            \Redirect::to($_POST['returnURI'] ?? '/admin/platby/structure/group');
        }
        $_POST['type'] = $data['pg_type'];
        $_POST['name'] = $data['pg_name'];
        $_POST['description'] = $data['pg_description'];
        $_POST['base'] = $data['pg_base'];
        return static::displayForm('edit', $id);
    }

    public static function editPost($id)
    {
        \Permissions::checkError('platby', P_OWNED);
        if (!\DBPlatbyGroup::getSingle($id)) {
            \Message::warning('Kategorie s takovým ID neexistuje');
            \Redirect::to($_POST['returnURI'] ?? '/admin/platby/structure/group');
        }
        $form = static::checkData();
        if (!$form->isValid()) {
            \Message::warning($form->getMessages());
            return static::displayForm('edit', $id);
        }
        \DBPlatbyGroup::update(
            $id,
            $_POST['type'],
            $_POST['name'],
            $_POST['description'],
            $_POST['base']
        );

        $categoryOld = array_map(
            fn($item) => $item['pc_id'],
            \DBPlatbyGroup::getSingleWithCategories($id)
        );
        $categoryNew = $_POST['category'] ?? [];
        foreach (array_diff($categoryOld, $categoryNew) as $removed) {
            \DBPlatbyGroup::removeChild($id, $removed);
        }
        foreach (array_diff($categoryNew, $categoryOld) as $added) {
            \DBPlatbyGroup::addChild($id, $added);
        }

        $skupinyOld = array_map(
            fn($item) => $item['s_id'],
            \DBPlatbyGroup::getSingleWithSkupiny($id)
        );
        $skupinyNew = $_POST['skupiny'] ?? [];
        foreach (array_diff($skupinyOld, $skupinyNew) as $removed) {
            \DBSkupiny::removeChild($removed, $id);
        }
        foreach (array_diff($skupinyNew, $skupinyOld) as $added) {
            \DBSkupiny::addChild($added, $id);
        }

        \Redirect::to($_POST['returnURI'] ?? '/admin/platby/structure/group');
    }

    public static function remove($id)
    {
        \Permissions::checkError('platby', P_OWNED);
        if (!$data = \DBPlatbyGroup::getSingle($id)) {
            \Message::warning('Kategorie s takovým ID neexistuje');
            \Redirect::to($_POST['returnURI'] ?? '/admin/platby/structure/group');
        }
        if (static::getLinkedObjects($id)) {
            \Message::info(
                'Nelze odstranit kategorii s připojenými skupinami nebo specifickými symboly! '
                . '<form method=post>' . \Utils::submit('Odstranit spojení?', 'action', 'unlink')
                . '</form>'
            );
        }
        \Render::page('files/View/Admin/Platby/StructureGroupRemove.inc', [
            'header' => 'Správa plateb',
            'subheader' => 'Kategorie plateb',
            'id' => $id,
            'name' => $data['pg_name'],
            'returnURI' => $_SERVER['HTTP_REFERER'],
        ]);
    }

    public static function removePost($id)
    {
        \Permissions::checkError('platby', P_OWNED);
        if (!$data = \DBPlatbyGroup::getSingle($id)) {
            \Message::warning('Kategorie s takovým ID neexistuje');
            \Redirect::to($_POST['returnURI'] ?? '/admin/platby/structure/group');
        }

        $f = static::getLinkedObjects($id);
        if ($_POST['action'] == 'unlink') {
            $categoryCount = 0;
            foreach ($f['categories'] as $data) {
                \DBPlatbyGroup::removeChild($id, $data['pc_id']);
                ++$categoryCount;
            }

            $skupinaCount = 0;
            foreach ($f['skupiny'] as $data) {
                \DBSkupiny::removeChild($data['s_id'], $id);
                ++$skupinaCount;
            }
            \Message::info("Spojení s $skupinaCount skupinami a s $categoryCount kategoriemi bylo odstraněno");
            \Redirect::to("/admin/platby/structure/group/remove/$id");
        }
        if ($f) {
            \Redirect::to("/admin/platby/structure/group/remove/$id");
        }
        \DBPlatbyGroup::delete($id);
        \Redirect::to($_POST['returnURI'] ?? '/admin/platby/structure/group');
    }

    private static function getLinkedObjects($id)
    {
        $cat = \DBPlatbyGroup::getSingleWithCategories($id);
        $sku = \DBPlatbyGroup::getSingleWithSkupiny($id);

        if (empty($cat) && empty($sku)) {
            return [];
        } else {
            return ['categories' => $cat, 'skupiny' => $sku];
        }
    }

    private static function displayForm($action, $id = 0)
    {
        $data = \DBPlatbyGroup::getSingle($id);
        $categoriesSelected = array_flip(
            array_map(
                fn($item) => $item['pc_id'],
                \DBPlatbyGroup::getSingleWithCategories($id)
            )
        );
        $categories = array_map(
            fn($item) => [
                'buttons' => \Utils::checkbox('category[]', $item['pc_id'], isset($categoriesSelected[$item['pc_id']])),
                'name' => $item['pc_name'],
                'specific' => $item['pc_symbol'],
                'amount' => ((float) $item['pc_amount'] * (float) $data['pg_base']),
                'dueDate' => (new \Date($item['pc_date_due']))->getHumanDate(),
                'validDate' => \Format::range($item['pc_valid_from'], $item['pc_valid_to']),
                'usePrefix' => '&nbsp;' . ($item['pc_use_prefix'] ? '&#10003;' : '&#10799;'),
                'useBase' => '&nbsp;' . ($item['pc_use_base'] ? '&#10003;' : '&#10799;'),
                'archive' => '&nbsp;' . ($item['pc_archive'] ? '&#10003;' : '&#10799;')
            ],
            \DBPlatbyCategory::get(false)
        );

        $skupinySelected = array_flip(
            array_map(
                fn($item) => $item['s_id'],
                \DBPlatbyGroup::getSingleWithSkupiny($id)
            )
        );
        $skupiny = array_map(
            fn($item) => [
                'buttons' => \Utils::checkbox('skupiny[]', $item['s_id'], isset($skupinySelected[$item['s_id']])),
                'name' => "<div class=\"box\" title=\"{$item['s_description']}\" style=\"background-color:{$item['s_color_rgb']}\"></div>&nbsp;" . $item['s_name']
            ],
            \DBSkupiny::get()
        );

        \Render::page('files/View/Admin/Platby/StructureGroupForm.inc', [
            'header' => 'Správa plateb',
            'subheader' => $action == 'add' ? 'Přidat kategorii' : 'Upravit kategorii',
            'id' => $id,
            'action' => $action,
            'category' => $categories,
            'skupiny' => $skupiny,
            'returnURI' => $_SERVER['HTTP_REFERER'],
            'name' => $_POST['name'] ?? '',
            'type' => $_POST['type'] ?? '',
            'description' => $_POST['description'] ?? '',
            'base' => $_POST['base'] ?? '',
        ]);
    }

    protected static function checkData(): \Form
    {
        $f = new \Form();
        $f->checkInArray($_POST['type'], ['0', '1'], 'Neplatný typ kategorie');
        $f->checkNotEmpty($_POST['name'], 'Zadejte nějaký název platby');
        $f->checkNumeric($_POST['base'], 'Násobitel musí být zadán pouze čisly');
        return $f;
    }
}
