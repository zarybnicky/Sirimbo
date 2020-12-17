<?php
namespace Olymp\Controller\Admin;

class Skupiny
{
    public static function list()
    {
        \Permissions::checkError('skupiny', P_OWNED);
        $data = array_map(
            fn($item) => [
                'buttons' => \Buttons::skupiny($item['s_id']),
                'colorBox' => new \ColorboxHelper($item['s_color_rgb'], $item['s_description']),
                'name' => $item['s_name']
            ],
            \DBSkupiny::get()
        );
        new \RenderHelper('files/View/Admin/Skupiny/Overview.inc', [
            'header' => 'Správa skupin',
            'data' => $data
        ]);
    }

    public static function add()
    {
        \Permissions::checkError('skupiny', P_OWNED);
        return static::displayForm(0, 'add');
    }

    public static function addPost()
    {
        \Permissions::checkError('skupiny', P_OWNED);
        $form = static::checkData();
        if (!$form->isValid()) {
            new \MessageHelper('warning', $form->getMessages());
            return static::displayForm(0, 'add');
        }
        \DBSkupiny::insert($_POST['name'], $_POST['color'], $_POST['desc']);
        $insertId = \DBSkupiny::getInsertId();
        foreach ($_POST['group'] ?: [] as $item) {
            \DBSkupiny::addChild($insertId, $item);
        }
        new \RedirectHelper('/admin/skupiny');
    }

    public static function edit($id)
    {
        \Permissions::checkError('skupiny', P_OWNED);
        if (!$data = \DBSkupiny::getSingle($id)) {
            new \MessageHelper('warning', 'Skupina s takovým ID neexistuje');
            new \RedirectHelper('/admin/skupiny');
        }
        return static::displayForm($id, 'edit', $data);
    }

    public static function editPost($id)
    {
        \Permissions::checkError('skupiny', P_OWNED);
        if (!$data = \DBSkupiny::getSingle($id)) {
            new \MessageHelper('warning', 'Skupina s takovým ID neexistuje');
            new \RedirectHelper('/admin/skupiny');
        }
        $form = static::checkData();
        if (!$form->isValid()) {
            new \MessageHelper('warning', $form->getMessages());
            return static::displayForm($id, 'edit', $data);
        }
        \DBSkupiny::update($id, $_POST['name'], $_POST['color'], $_POST['desc']);

        $groupsOld = array_map(fn($item) => $item['pg_id'], \DBSkupiny::getSingleWithGroups($id));
        $groupsNew = $_POST['group'] ?: [];
        foreach (array_diff($groupsOld, $groupsNew) as $removed) {
            \DBSkupiny::removeChild($id, $removed);
        }
        foreach (array_diff($groupsNew, $groupsOld) as $added) {
            \DBSkupiny::addChild($id, $added);
        }
        new \RedirectHelper('/admin/skupiny');
    }

    public static function remove($id)
    {
        \Permissions::checkError('skupiny', P_OWNED);
        if (!$data = \DBSkupiny::getSingle($id)) {
            new \MessageHelper('warning', 'Skupina s takovým ID neexistuje');
            new \RedirectHelper('/admin/skupiny');
        }
        if (static::getLinkedSkupinaObjects($id)) {
            new \MessageHelper(
                'info',
                'Nemůžu odstranit skupinu s připojenými kategoriemi! ' . new \Tag(
                    'form',
                    ['action' => '', 'mthod' => 'post'],
                    (new \SubmitHelper('Odstranit spojení?'))->data('action', 'unlink')
                )
            );
        }
        return new \RenderHelper('files/View/Admin/RemovePrompt.inc', [
            'header' => 'Správa skupin',
            'prompt' => 'Opravdu chcete odstranit skupinu?',
            'returnURI' => $_SERVER['HTTP_REFERER'] ?: '/admin/skupiny',
            'data' => [['id' => $data['s_id'], 'text' => $data['s_name']]]
        ]);
    }

    public static function removePost($id)
    {
        \Permissions::checkError('skupiny', P_OWNED);
        if (!$data = \DBSkupiny::getSingle($id)) {
            new \MessageHelper('warning', 'Skupina s takovým ID neexistuje');
            new \RedirectHelper('/admin/skupiny');
        }
        if ($_POST['action'] == 'unlink') {
            $f = static::getLinkedSkupinaObjects($id);
            $groupCount = 0;
            foreach ($f['groups'] as $data) {
                \DBSkupiny::removeChild($id, $data['pg_id']);
                ++$groupCount;
            }
            new \MessageHelper('info', "Spojení s $groupCount kategoriemi byla odstraněna.");
            return new \RedirectHelper('/admin/skupiny/remove/' . $id);
        }
        if (static::getLinkedSkupinaObjects($id)) {
            return new \RedirectHelper('/admin/skupiny/remove/' . $id);
        }
        \DBSkupiny::delete($id);
        new \RedirectHelper('/admin/skupiny');
    }

    private static function displayForm($id, $action, $data = null)
    {
        $groupsSelected = array_flip(
            array_map(fn($item) => $item['pg_id'], \DBSkupiny::getSingleWithGroups($id))
        );

        $groups = array_map(
            fn($item) => [
                'buttons' => new \CheckboxHelper('group[]', $item['pg_id'], isset($groupsSelected[$item['pg_id']])),
                'type' => $item['pg_type'] == '1' ? 'Členské příspěvky' : 'Běžné platby',
                'name' => $item['pg_name'],
                'base' => $item['pg_base']
            ],
            \DBPlatbyGroup::getGroups()
        );

        new \RenderHelper('files/View/Admin/Skupiny/Form.inc', [
            'header' => 'Správa skupin',
            'subheader' => $action == 'add' ? 'Přidat skupinu' : 'Upravit skupinu',
            'id' => $id,
            'name' => $_POST['name'] ?: ($data ? $data['s_name'] : ''),
            'color' => $_POST['color'] ?: ($data ? $data['s_color_rgb'] : ''),
            'popis' => $_POST['popis'] ?: ($data ? $data['s_description'] : ''),
            'action' => $action,
            'groups' => $groups
        ]);
    }

    private static function getLinkedSkupinaObjects($id)
    {
        $group = \DBSkupiny::getSingleWithGroups($id);
        return $group ? ['groups' => $group] : [];
    }

    private static function checkData(): \Form
    {
        $f = new \Form();
        $f->checkNotEmpty($_POST['name'], 'Zadejte prosím nějaké jméno.');
        $f->checkNotEmpty($_POST['desc'], 'Zadejte prosím nějaký popis.');
        $f->checkRegexp($_POST['color'], '/#[0-9a-f]{6}/i', 'Zadejte prosím platnou barvu.');
        return $f;
    }
}
