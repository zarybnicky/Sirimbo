<?php
namespace Olymp\Controller\Admin;

class PlatbyGroup
{
    public static function list()
    {
        \Permissions::checkError('platby', P_OWNED);
        \Render::twig('Admin/PlatbyStructureGroup.twig', [
            'data' => \DBPlatbyGroup::getGroups(),
        ]);
    }

    public static function add()
    {
        \Permissions::checkError('platby', P_OWNED);
        $_POST['base'] = 1;
        return self::displayForm('add', 0);
    }

    public static function addPost()
    {
        \Permissions::checkError('platby', P_OWNED);
        $form = self::checkData();
        if (!$form->isValid()) {
            \Message::warning($form->getMessages());
            return self::displayForm('add', 0);
        }
        \Database::query(
            "INSERT INTO platby_group
            (pg_type,pg_name,pg_description, pg_base)
            VALUES ('?','?','?','?')",
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
        \Redirect::to($_POST['returnURI'] ?? '/platby/structure/group');
    }

    public static function edit($id)
    {
        \Permissions::checkError('platby', P_OWNED);
        if (!$data = \DBPlatbyGroup::getSingle($id)) {
            \Message::warning('Kategorie s takovým ID neexistuje');
            \Redirect::to($_POST['returnURI'] ?? '/platby/structure/group');
        }
        $_POST['type'] = $data['pg_type'];
        $_POST['name'] = $data['pg_name'];
        $_POST['description'] = $data['pg_description'];
        $_POST['base'] = $data['pg_base'];
        return self::displayForm('edit', $id);
    }

    public static function editPost($id)
    {
        \Permissions::checkError('platby', P_OWNED);
        if (!\DBPlatbyGroup::getSingle($id)) {
            \Message::warning('Kategorie s takovým ID neexistuje');
            \Redirect::to($_POST['returnURI'] ?? '/platby/structure/group');
        }
        $form = self::checkData();
        if (!$form->isValid()) {
            \Message::warning($form->getMessages());
            return self::displayForm('edit', $id);
        }
        \Database::query(
            "UPDATE platby_group
            SET pg_type='?',pg_name='?',pg_description='?',pg_base='?'
            WHERE pg_id='?'",
            $_POST['type'],
            $_POST['name'],
            $_POST['description'],
            $_POST['base'],
            $id,
        );

        $categoryOld = array_column(\DBPlatbyGroup::getSingleWithCategories($id), 'pc_id');
        $categoryNew = $_POST['category'] ?? [];
        foreach (array_diff($categoryOld, $categoryNew) as $removed) {
            \DBPlatbyGroup::removeChild($id, $removed);
        }
        foreach (array_diff($categoryNew, $categoryOld) as $added) {
            \DBPlatbyGroup::addChild($id, $added);
        }

        $skupinyOld = array_column(\DBPlatbyGroup::getSingleWithSkupiny($id), 's_id');
        $skupinyNew = $_POST['skupiny'] ?? [];
        foreach (array_diff($skupinyOld, $skupinyNew) as $removed) {
            \DBSkupiny::removeChild($removed, $id);
        }
        foreach (array_diff($skupinyNew, $skupinyOld) as $added) {
            \DBSkupiny::addChild($added, $id);
        }

        \Redirect::to($_POST['returnURI'] ?? '/platby/structure/group');
    }

    public static function remove($id)
    {
        \Permissions::checkError('platby', P_OWNED);
        if (!$data = \DBPlatbyGroup::getSingle($id)) {
            \Message::warning('Kategorie s takovým ID neexistuje');
            \Redirect::to($_POST['returnURI'] ?? '/platby/structure/group');
        }
        if (self::getLinkedObjects($id)) {
            \Message::info(
                'Nelze odstranit kategorii s připojenými skupinami nebo specifickými symboly! '
                . '<form method=post>'
                . '<button class="btn btn-primary" name="action" value="unlink">Odstranit spojení?</button>'
                . '</form>'
            );
        }
        \Render::twig('Admin/PlatbyStructureGroupRemove.twig', [
            'id' => $id,
            'name' => $data['pg_name'],
            'returnURI' => $_SERVER['HTTP_REFERER'],
        ]);
    }

    public static function removePost($id)
    {
        \Permissions::checkError('platby', P_OWNED);
        if (!\DBPlatbyGroup::getSingle($id)) {
            \Message::warning('Kategorie s takovým ID neexistuje');
            \Redirect::to($_POST['returnURI'] ?? '/platby/structure/group');
        }

        $f = self::getLinkedObjects($id);
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
            \Redirect::to("/platby/structure/group/remove/$id");
        }
        if ($f) {
            \Redirect::to("/platby/structure/group/remove/$id");
        }
        \Database::query("DELETE FROM platby_group WHERE pg_id='?'", $id);
        \Redirect::to($_POST['returnURI'] ?? '/platby/structure/group');
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
        \Render::twig('Admin/PlatbyStructureGroupForm.twig', [
            'id' => $id,
            'action' => $action,
            'data' => \DBPlatbyGroup::getSingle($id),
            'skupiny' => \DBSkupiny::get(),
            'category' => \DBPlatbyCategory::get(false),
            'skupinySelected' => array_column(\DBPlatbyGroup::getSingleWithSkupiny($id), 's_id', 's_id'),
            'categoriesSelected' => array_column(\DBPlatbyGroup::getSingleWithCategories($id), 'pc_id', 'pc_id'),
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