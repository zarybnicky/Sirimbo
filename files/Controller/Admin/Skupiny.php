<?php
require_once 'files/Controller/Admin.php';
class Controller_Admin_Skupiny extends Controller_Admin
{
    public function __construct()
    {
        Permissions::checkError('skupiny', P_OWNED);
    }
    public function view($request)
    {
        switch($request->post('action')) {
        case 'edit':
            $skupiny = $request->post('data');
            if ($skupiny[0]) {
                $this->redirect('/admin/skupiny/edit/' . $skupiny[0]);
            }
            break;
        case 'remove':
            if (!is_array($request->post('data'))) {
                break;
            }
            $this->redirect(
                '/admin/skupiny/remove?'
                . http_build_query(
                    array('u' => $request->post('data'))
                )
            );
            break;
        }
        $data = array_map(
            function ($item) {
                return array(
                    'buttons' => ($this->getEditLink('/admin/skupiny/edit/' . $item['s_id'])
                                  . $this->getRemoveLink('/admin/skupiny/remove/' . $item['s_id'])),
                    'colorBox' => $this->colorbox($item['s_color_rgb'], $item['s_description'])
                                       ->render(),
                    'name' => $item['s_name']
                );
            },
            DBSkupiny::get()
        );
        $this->render(
            'files/View/Admin/Skupiny/Overview.inc',
            array(
                'showMenu' => !TISK,
                'data' => $data
            )
        );
    }

    public function add($request)
    {
        if (!$request->post()) {
            $this->displayForm($request);
            return;
        }
        if (is_object($f = $this->checkPost($request))) {
            $this->redirect()->setMessage($f->getMessages());
            $this->displayForm($request);
            return;
        }
        DBSkupiny::insert(
            $request->post('name'),
            $request->post('color'),
            $request->post('desc')
        );
        $insertId = DBSkupiny::getInsertId();

        if ($request->get('group') &&
            ($data = DBPlatbyGroup::getSingle($request->get('group')))
        ) {
            DBSkupiny::addChild($insertId, $request->get('group'));
            $conflicts = DBPlatby::checkConflicts($insertId);
            if ($conflicts) {
                DBSkupiny::removeChild($insertId, $request->get('group'));
                $this->redirect(
                    '/admin/platby/structure/group/edit/' . $request->get('group'),
                    'Skupina byla přidána, ale nebyla přiřazena - takové přiřazení není platné.'
                );
            }
            $this->redirect(
                '/admin/platby/structure/group/edit/' . $request->get('group'),
                'Skupina úspěšně přidána a přiřazena'
            );
        }
        $this->redirect('/admin/skupiny', 'Skupina úspěšně přidána');
    }

    public function edit($request)
    {
        $id = $request->getId();
        if (!$id || !($data = DBSkupiny::getSingle($id))) {
            $this->redirect('/admin/skupiny', 'Skupina s takovým ID neexistuje');
        }

        if ($request->post('action') == 'group') {
            $data = DBPlatbyGroup::getSingle($request->post('group'));
            if (!$data) {
                $this->redirect(
                    '/admin/skupiny/edit/' . $id,
                    'Kategorie s takovým ID neexistuje.'
                );
            }

            DBSkupiny::addChild($id, $request->post('group'));
            $conflicts = DBPlatby::checkConflicts($id);

            if (!empty($conflicts)) {
                DBSkupiny::removeChild($id, $request->post('group'));
                $this->redirect(
                    '/admin/skupiny/edit/' . $id,
                    'Takové přiřazení není platné - způsobilo by, že jeden specifický symbol by byl v jedné skupině dvakrát.'
                );
            }
            $this->redirect(
                '/admin/skupiny/edit/' . $id,
                'Kategorie byla úspěšně přiřazena.'
            );
        } elseif ($request->post('action') == 'group_remove') {
            $data = DBPlatbyGroup::getSingle($request->post('group'));
            if (!$data) {
                $this->redirect(
                    '/admin/skupiny/edit/' . $id,
                    'Kategorie s takovým ID neexistuje.'
                );
            }

            DBSkupiny::removeChild($id, $request->post('group'));
            $this->redirect(
                '/admin/skupiny/edit/' . $id,
                'Spojení s kategorií bylo úspěšně odstraněno.'
            );
        }

        if (!$request->post()) {
            $this->displayForm($request, $data);
            return;
        }

        if (is_object($f = $this->checkPost($request))) {
            $this->redirect()->setMessage($f->getMessages());
            $this->displayForm($request, $data);
            return;
        }

        DBSkupiny::update(
            $id,
            $request->post('name'),
            $request->post('color'),
            $request->post('desc')
        );
        $this->redirect('/admin/skupiny', 'Skupina úspěšně upravena');
    }

    public function remove($request)
    {
        $id = $request->getId();
        if (!$id || !($data = DBSkupiny::getSingle($id))) {
            $this->redirect('/admin/skupiny', 'Skupina s takovým ID neexistuje');
        }

        if ($request->post('action') == 'unlink') {
            $f = $this->getLinkedSkupinaObjects($id);

            $groupCount = 0;
            foreach ($f['groups'] as $data) {
                DBSkupiny::removeChild($id, $data['pg_id']);
                ++$groupCount;
            }

            $this->redirect(
                '/admin/platby/structure/category/remove/' . $id,
                'Spojení s ' . $groupCount . ' kategoriemi byla odstraněna.'
            );
            return;
        }
        if (((!$request->post() || $request->post('action') == 'confirm') &&
            ($f = $this->getLinkedSkupinaObjects($id))) || !$request->post()
        ) {
            if (isset($f) && $f) {
                $this->redirect()->setMessage(
                    'Nemůžu odstranit skupinu s připojenými kategoriemi! '
                    . '<form action="" method="post">'
                    . $this->submit('Odstranit spojení?')->data('action', 'unlink')
                    . '</form>'
                );
            }
            $this->render(
                'files/View/Admin/RemovePrompt.inc',
                array(
                    'header' => 'Správa skupin',
                    'prompt' => 'Opravdu chcete odstranit skupinu?',
                    'returnURI' => $request->getReferer(),
                    'data' => array(
                        array(
                            'id' => $data['s_id'],
                            'text' => $data['s_name']
                        )
                    )
                )
            );
            return;
        }
        DBSkupiny::delete($id);
        $this->redirect('/admin/skupiny', 'Skupina byla úspěšně odebrána.');
    }

    private function displayForm($request, $data = null)
    {
        $id = $request->getId() ?: '0';
        $action = $request->getAction();

        $groups = array_map(
            function ($item) {
                return array(
                    'buttons' => '<form action="" method="post">'
                    . $this->getUnlinkGroupButton($item['pg_id'])
                    . $this->getEditLink('/admin/platby/structure/group/edit/' . $item['pg_id'])
                    . $this->getRemoveLink('/admin/platby/structure/group/remove/' . $item['pg_id'])
                    . '</form>',
                    'type' => ($item['pg_type'] == '1'
                               ? 'Členské příspěvky'
                               : 'Běžné platby'),
                    'name' => $item['pg_name'],
                    'base' => $item['pg_base']
                );
            },
            DBSkupiny::getSingleWithGroups($id)
        );

        $groupNotInSkupina = DBPlatbyGroup::getNotInSkupina($id);
        $groupSelect = array();
        foreach ($groupNotInSkupina as $array) {
            $groupSelect[$array['pg_id']] = $array['pg_name'];
        }

        $this->render(
            'files/View/Admin/Skupiny/Form.inc',
            array(
                'id' => $id,
                'name' => $request->post('name') ?: $data ? $data['s_name'] : '',
                'color' => $request->post('color') ?: $data ? $data['s_color_rgb'] : '',
                'popis' => $request->post('popis') ?: $data ? $data['s_description'] : '',
                'action' => $action,
                'header' => $action == 'add' ? 'Přidat skupinu' : 'Upravit skupinu',
                'groups' => $groups,
                'groupSelect' => $groupSelect
            )
        );
    }

    private function getLinkedSkupinaObjects($id)
    {
        $group = DBSkupiny::getSingleWithGroups($id);

        if (empty($group)) {
            return array();
        } else {
            return array('groups' => $group);
        }
    }
    private function checkPost($request)
    {
        $f = new Form();

        $f->checkNotEmpty($request->post('name'), 'Zadejte prosím nějaké jméno.');
        $f->checkNotEmpty($request->post('desc'), 'Zadejte prosím nějaký popis.');
        $f->checkRegexp($request->post('color'), '/#[0-9a-f]{6}/i', 'Zadejte prosím platnou barvu.');

        return $f->isValid() ? true : $f;
    }
    private function getEditLink($link)
    {
        return '<a href="' . $link . '"><img alt="Upravit" src="/images/wrench.png" /></a>';
    }
    private function getRemoveLink($link)
    {
        return '<a href="' . $link . '"><img alt="Odstranit" src="/images/cross.png" /></a>';
    }
    private function getUnlinkGroupButton($id)
    {
        return $this->hidden('group', $id)
            . $this->submit('<img alt="Odstranit spojení" src="/images/unlink.png" />')
                   ->data('action', 'group_remove');
    }
}