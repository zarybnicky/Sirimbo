<?php
class Controller_Admin_Platby_Structure_Group extends Controller_Admin_Platby
{
    public function view($request)
    {
        \Permissions::checkError('platby', P_OWNED);
        new \RenderHelper('files/View/Admin/Platby/StructureGroupOverview.inc', [
            'header' => 'Správa plateb',
            'subheader' => 'Kategorie plateb',
            'data' => static::getGroups(),
            'uri' => trim(explode('?', $_SERVER['REQUEST_URI'])[0], '/')
        ]);
    }

    protected static function getGroups()
    {
        return array_map(
            fn($item) => [
                'name' => $item['pg_name'],
                'type' => $item['pg_type'] ? 'Členské příspěvky' : 'Běžné platby',
                'buttons' => (
                    new \EditLinkHelper('/admin/platby/structure/group/edit/' . $item['pg_id']) .
                    new \RemoveLinkHelper('/admin/platby/structure/group/remove/' . $item['pg_id'])
                )
            ],
            \DBPlatbyGroup::getGroups()
        );
    }

    public function add($request)
    {
        \Permissions::checkError('platby', P_OWNED);
        if (!$_POST) {
            $_POST['base'] = 1;
            return static::displayForm($request, 'add', 0);
        }
        $form = static::checkData($request);
        if (!$form->isValid()) {
            new \MessageHelper('warning', $form->getMessages());
            return static::displayForm($request, 'add', 0);
        }
        \DBPlatbyGroup::insert(
            $_POST['type'],
            $_POST['name'],
            $_POST['description'],
            $_POST['base']
        );
        $insertId = \DBPlatbyGroup::getInsertId();

        foreach ($_POST['category'] ?: [] as $item) {
            \DBPlatbyGroup::addChild($insertId, $item);
        }
        foreach ($_POST['skupiny'] ?: [] as $item) {
            \DBSkupiny::addChild($item, $insertId);
        }

        new \RedirectHelper($_POST['returnURI'] ?: '/admin/platby/structure/group');
    }

    public function edit($request)
    {
        \Permissions::checkError('platby', P_OWNED);
        if (!$id = $request->getId()) {
            new \MessageHelper('warning', 'Kategorie s takovým ID neexistuje');
            new \RedirectHelper($_POST['returnURI'] ?: '/admin/platby/structure/group');
        }
        if (!$data = \DBPlatbyGroup::getSingle($id)) {
            new \MessageHelper('warning', 'Kategorie s takovým ID neexistuje');
            new \RedirectHelper($_POST['returnURI'] ?: '/admin/platby/structure/group');
        }

        if (!$_POST) {
            $_POST['type'] = $data['pg_type'];
            $_POST['name'] = $data['pg_name'];
            $_POST['description'] = $data['pg_description'];
            $_POST['base'] = $data['pg_base'];
            return static::displayForm($request, 'edit', $id);
        }
        $form = static::checkData($request);
        if (!$form->isValid()) {
            new \MessageHelper('warning', $form->getMessages());
            return static::displayForm($request, 'edit', $id);
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
        $categoryNew = $_POST['category'] ?: [];
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
        $skupinyNew = $_POST['skupiny'] ?: [];
        foreach (array_diff($skupinyOld, $skupinyNew) as $removed) {
            \DBSkupiny::removeChild($removed, $id);
        }
        foreach (array_diff($skupinyNew, $skupinyOld) as $added) {
            \DBSkupiny::addChild($added, $id);
        }

        new \RedirectHelper($_POST['returnURI'] ?: '/admin/platby/structure/group');
    }

    public function remove($request)
    {
        \Permissions::checkError('platby', P_OWNED);
        if (!$id = $request->getId()) {
            new \MessageHelper('warning', 'Kategorie s takovým ID neexistuje');
            new \RedirectHelper($_POST['returnURI'] ?: '/admin/platby/structure/group');
        }
        if (!$data = \DBPlatbyGroup::getSingle($id)) {
            new \MessageHelper('warning', 'Kategorie s takovým ID neexistuje');
            new \RedirectHelper($_POST['returnURI'] ?: '/admin/platby/structure/group');
        }

        if ($_POST['action'] == 'unlink') {
            $f = static::getLinkedObjects($id);

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
            new \MessageHelper(
                'info',
                'Spojení s ' . $skupinaCount . ' skupinami a s '
                . $categoryCount . ' kategoriemi bylo odstraněno'
            );
            return new \RedirectHelper('/admin/platby/structure/group/remove/' . $id);
        }
        if (((!$_POST || $_POST['action'] == 'confirm') && ($f = static::getLinkedObjects($id))) || !$_POST) {
            if (isset($f) && $f) {
                new \MessageHelper(
                    'info',
                    'Nelze odstranit kategorii s připojenými skupinami nebo specifickými symboly! '
                    . new Tag(
                        'form',
                        ['action' => '', 'method' => 'post'],
                        (new \SubmitHelper('Odstranit spojení?'))->data('action', 'unlink')
                    )
                );
            }
            return new \RenderHelper('files/View/Admin/Platby/StructureGroupRemove.inc', [
                'header' => 'Správa plateb',
                'subheader' => 'Kategorie plateb',
                'id' => $id,
                'name' => $data['pg_name'],
                'returnURI' => $_SERVER['HTTP_REFERER'],
                'uri' => trim(explode('?', $_SERVER['REQUEST_URI'])[0], '/')
            ]);
        }
        \DBPlatbyGroup::delete($id);
        new \RedirectHelper($_POST['returnURI'] ?: '/admin/platby/structure/group');
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

    private static function displayForm($request, $action, $id = 0)
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
                'buttons' => new \CheckboxHelper('category[]', $item['pc_id'], isset($categoriesSelected[$item['pc_id']])),
                'name' => $item['pc_name'],
                'specific' => $item['pc_symbol'],
                'amount' => ((float) $item['pc_amount'] * (float) $data['pg_base']),
                'dueDate' => (new \Date($item['pc_date_due']))->getHumanDate(),
                'validDate' => formatRange($item['pc_valid_from'], $item['pc_valid_to']),
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
                'buttons' => new \CheckboxHelper('skupiny[]', $item['s_id'], isset($skupinySelected[$item['s_id']])),
                'name' => new \ColorboxHelper($item['s_color_rgb'], $item['s_description']) . '&nbsp;' . $item['s_name']
            ],
            \DBSkupiny::get()
        );

        new \RenderHelper('files/View/Admin/Platby/StructureGroupForm.inc', [
            'header' => 'Správa plateb',
            'subheader' => $action == 'add' ? 'Přidat kategorii' : 'Upravit kategorii',
            'id' => $id,
            'action' => $action,
            'category' => $categories,
            'skupiny' => $skupiny,
            'returnURI' => $_SERVER['HTTP_REFERER'],
            'name' => $_POST['name'] ?: '',
            'type' => $_POST['type'] ?: '',
            'description' => $_POST['description'] ?: '',
            'base' => $_POST['base'] ?: '',
            'uri' => trim(explode('?', $_SERVER['REQUEST_URI'])[0], '/')
        ]);
    }

    protected static function checkData($request): \Form
    {
        $f = new \Form();
        $f->checkInArray($_POST['type'], ['0', '1'], 'Neplatný typ kategorie');
        $f->checkNotEmpty($_POST['name'], 'Zadejte nějaký název platby');
        $f->checkNumeric($_POST['base'], 'Násobitel musí být zadán pouze čisly');
        return $f;
    }
}
