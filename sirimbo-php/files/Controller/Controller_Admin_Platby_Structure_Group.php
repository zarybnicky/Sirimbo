<?php
class Controller_Admin_Platby_Structure_Group extends Controller_Admin_Platby
{
    public function __construct($request)
    {
        parent::__construct($request);
        Permissions::checkError('platby', P_OWNED);
    }

    public function view($request)
    {
        new \RenderHelper('files/View/Admin/Platby/StructureGroupOverview.inc', [
            'header' => 'Správa plateb',
            'subheader' => 'Kategorie plateb',
            'data' => $this->getGroups(),
            'uri' => $request->getLiteralURI()
        ]);
    }

    protected function getGroups()
    {
        return array_map(
            function ($item) {
                return [
                    'name' => $item['pg_name'],
                    'type' => $item['pg_type'] ? 'Členské příspěvky' : 'Běžné platby',
                    'buttons' => (
                        new EditLinkHelper('/admin/platby/structure/group/edit/' . $item['pg_id'])
                        . new RemoveLinkHelper('/admin/platby/structure/group/remove/' . $item['pg_id'])
                    )
                ];
            },
            DBPlatbyGroup::getGroups()
        );
    }

    public function add($request)
    {
        if (!$request->post()) {
            $request->post('base', 1);
            return $this->displayForm($request, 'add', 0);
        }
        $form = $this->checkData($request);
        if (!$form->isValid()) {
            new \MessageHelper('warning', $form->getMessages());
            return $this->displayForm($request, 'add', 0);
        }
        DBPlatbyGroup::insert(
            $request->post('type'),
            $request->post('name'),
            $request->post('description'),
            $request->post('base')
        );
        $insertId = DBPlatbyGroup::getInsertId();

        foreach ($request->post('category') ?: [] as $item) {
            DBPlatbyGroup::addChild($insertId, $item);
        }
        foreach ($request->post('skupiny') ?: [] as $item) {
            DBSkupiny::addChild($item, $insertId);
        }

        new \RedirectHelper($request->post('returnURI') ?: '/admin/platby/structure/group');
    }

    public function edit($request)
    {
        if (!$id = $request->getId()) {
            new \MessageHelper('warning', 'Kategorie s takovým ID neexistuje');
            new \RedirectHelper($request->post('returnURI') ?: '/admin/platby/structure/group');
        }
        if (!$data = DBPlatbyGroup::getSingle($id)) {
            new \MessageHelper('warning', 'Kategorie s takovým ID neexistuje');
            new \RedirectHelper($request->post('returnURI') ?: '/admin/platby/structure/group');
        }

        if (!$request->post()) {
            $request->post('type', $data['pg_type']);
            $request->post('name', $data['pg_name']);
            $request->post('description', $data['pg_description']);
            $request->post('base', $data['pg_base']);
            return $this->displayForm($request, 'edit', $id);
        }
        $form = $this->checkData($request);
        if (!$form->isValid()) {
            new \MessageHelper('warning', $form->getMessages());
            return $this->displayForm($request, 'edit', $id);
        }

        DBPlatbyGroup::update(
            $id,
            $request->post('type'),
            $request->post('name'),
            $request->post('description'),
            $request->post('base')
        );

        $categoryOld = array_map(
            function ($item) {
                return $item['pc_id'];
            },
            DBPlatbyGroup::getSingleWithCategories($id)
        );
        $categoryNew = $request->post('category') ?: [];
        foreach (array_diff($categoryOld, $categoryNew) as $removed) {
            DBPlatbyGroup::removeChild($id, $removed);
        }
        foreach (array_diff($categoryNew, $categoryOld) as $added) {
            DBPlatbyGroup::addChild($id, $added);
        }

        $skupinyOld = array_map(
            function ($item) {
                return $item['s_id'];
            },
            DBPlatbyGroup::getSingleWithSkupiny($id)
        );
        $skupinyNew = $request->post('skupiny') ?: [];
        foreach (array_diff($skupinyOld, $skupinyNew) as $removed) {
            DBSkupiny::removeChild($removed, $id);
        }
        foreach (array_diff($skupinyNew, $skupinyOld) as $added) {
            DBSkupiny::addChild($added, $id);
        }

        new \RedirectHelper($request->post('returnURI') ?: '/admin/platby/structure/group');
    }

    public function remove($request)
    {
        if (!$id = $request->getId()) {
            new \MessageHelper('warning', 'Kategorie s takovým ID neexistuje');
            new \RedirectHelper($request->post('returnURI') ?: '/admin/platby/structure/group');
        }
        if (!$data = DBPlatbyGroup::getSingle($id)) {
            new \MessageHelper('warning', 'Kategorie s takovým ID neexistuje');
            new \RedirectHelper($request->post('returnURI') ?: '/admin/platby/structure/group');
        }

        if ($request->post('action') == 'unlink') {
            $f = $this->getLinkedObjects($id);

            $categoryCount = 0;
            foreach ($f['categories'] as $data) {
                DBPlatbyGroup::removeChild($id, $data['pc_id']);
                ++$categoryCount;
            }

            $skupinaCount = 0;
            foreach ($f['skupiny'] as $data) {
                DBSkupiny::removeChild($data['s_id'], $id);
                ++$skupinaCount;
            }
            new \MessageHelper('info', 
                'Spojení s ' . $skupinaCount . ' skupinami a s '
                . $categoryCount . ' kategoriemi bylo odstraněno'
            );
            return new \RedirectHelper('/admin/platby/structure/group/remove/' . $id);
        }
        if (((!$request->post() || $request->post('action') == 'confirm')
            && ($f = $this->getLinkedObjects($id))) || !$request->post()
        ) {
            if (isset($f) && $f) {
                new \MessageHelper('info', 
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
                'returnURI' => $request->getReferer(),
                'uri' => $request->getLiteralURI()
            ]);
        }
        DBPlatbyGroup::delete($id);
        new \RedirectHelper($request->post('returnURI') ?: '/admin/platby/structure/group');
    }

    private function getLinkedObjects($id)
    {
        $cat = DBPlatbyGroup::getSingleWithCategories($id);
        $sku = DBPlatbyGroup::getSingleWithSkupiny($id);

        if (empty($cat) && empty($sku)) {
            return [];
        } else {
            return ['categories' => $cat, 'skupiny' => $sku];
        }
    }

    private function displayForm($request, $action, $id = 0)
    {
        $data = DBPlatbyGroup::getSingle($id);
        $categoriesSelected = array_flip(
            array_map(
                function ($item) {
                    return $item['pc_id'];
                },
                DBPlatbyGroup::getSingleWithCategories($id)
            )
        );
        $categories = array_map(
            function ($item) use ($categoriesSelected, $data) {
                return [
                    'buttons' => new \CheckboxHelper('category[]', $item['pc_id'], isset($categoriesSelected[$item['pc_id']])),
                    'name' => $item['pc_name'],
                    'specific' => $item['pc_symbol'],
                    'amount' => ((float) $item['pc_amount'] * (float) $data['pg_base']),
                    'dueDate' => (new Date($item['pc_date_due']))->getDate(Date::FORMAT_SIMPLE_SPACED),
                    'validDate' => formatRange($item['pc_valid_from'], $item['pc_valid_to']),
                    'usePrefix' => '&nbsp;' . ($item['pc_use_prefix'] ? '&#10003;' : '&#10799;'),
                    'useBase' => '&nbsp;' . ($item['pc_use_base'] ? '&#10003;' : '&#10799;'),
                    'archive' => '&nbsp;' . ($item['pc_archive'] ? '&#10003;' : '&#10799;')
                ];
            },
            DBPlatbyCategory::get(false)
        );

        $skupinySelected = array_flip(
            array_map(
                function ($item) {
                    return $item['s_id'];
                },
                DBPlatbyGroup::getSingleWithSkupiny($id)
            )
        );
        $skupiny = array_map(
            function ($item) use ($skupinySelected) {
                return [
                    'buttons' => new \CheckboxHelper('skupiny[]', $item['s_id'], isset($skupinySelected[$item['s_id']])),
                    'name' => new ColorboxHelper($item['s_color_rgb'], $item['s_description']) . '&nbsp;' . $item['s_name']
                ];
            },
            DBSkupiny::get()
        );

        new \RenderHelper('files/View/Admin/Platby/StructureGroupForm.inc', [
            'header' => 'Správa plateb',
            'subheader' => $action == 'add' ? 'Přidat kategorii' : 'Upravit kategorii',
            'id' => $id,
            'action' => $action,
            'category' => $categories,
            'skupiny' => $skupiny,
            'returnURI' => $request->getReferer(),
            'name' => $request->post('name') ?: '',
            'type' => $request->post('type') ?: '',
            'description' => $request->post('description') ?: '',
            'base' => $request->post('base') ?: '',
            'uri' => $request->getLiteralURI()
        ]);
    }

    protected function checkData($request): Form
    {
        $f = new Form();
        $f->checkInArray($request->post('type'), ['0', '1'], 'Neplatný typ kategorie');
        $f->checkNotEmpty($request->post('name'), 'Zadejte nějaký název platby');
        $f->checkNumeric($request->post('base'), 'Násobitel musí být zadán pouze čisly');
        return $f;
    }
}
