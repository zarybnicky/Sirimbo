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
                    . http_build_query(['u' => $request->post('data')])
                );
                break;
        }
        $data = array_map(
            function ($item) {
                return [
                    'buttons' => (
                        $this->editLink('/admin/skupiny/edit/' . $item['s_id'])
                        . $this->removeLink('/admin/skupiny/remove/' . $item['s_id'])
                    ),
                    'colorBox' => $this->colorbox($item['s_color_rgb'], $item['s_description'])
                                       ->render(),
                    'name' => $item['s_name']
                ];
            },
            DBSkupiny::get()
        );
        $this->render(
            'files/View/Admin/Skupiny/Overview.inc',
            ['showMenu' => !TISK, 'data' => $data]
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

        foreach ($request->post('group') as $item) {
            DBSkupiny::addChild($insertId, $item);
        }

        $this->redirect('/admin/skupiny', 'Skupina úspěšně přidána');
    }

    public function edit($request)
    {
        $id = $request->getId();
        if (!$id || !($data = DBSkupiny::getSingle($id))) {
            $this->redirect('/admin/skupiny', 'Skupina s takovým ID neexistuje');
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

        $groupsOld = array_map(
            function ($item) {
                return $item['pg_id'];
            },
            DBSkupiny::getSingleWithGroups($id)
        );
        $groupsNew = $request->post('group') ?: [];
        foreach (array_diff($groupsOld, $groupsNew) as $removed) {
            DBSkupiny::removeChild($id, $removed);
        }
        foreach (array_diff($groupsNew, $groupsOld) as $added) {
            DBSkupiny::addChild($id, $added);
        }

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
                '/admin/skupiny/remove/' . $id,
                'Spojení s ' . $groupCount . ' kategoriemi byla odstraněna.'
            );
            return;
        }
        if (($f = $this->getLinkedSkupinaObjects($id)) || !$request->post()) {
            if (isset($f) && $f) {
                $this->redirect()->setMessage(
                    'Nemůžu odstranit skupinu s připojenými kategoriemi! '
                    . new Tag(
                        'form',
                        ['action' => '', 'mthod' => 'post'],
                        $this->submit('Odstranit spojení?')->data('action', 'unlink')
                    )
                );
            }
            $this->render(
                'files/View/Admin/RemovePrompt.inc',
                [
                    'header' => 'Správa skupin',
                    'prompt' => 'Opravdu chcete odstranit skupinu?',
                    'returnURI' => $request->getReferer() ?: '/admin/skupiny',
                    'data' => [['id' => $data['s_id'], 'text' => $data['s_name']]]
                ]
            );
            return;
        }
        DBSkupiny::delete($id);
        $this->redirect('/admin/skupiny', 'Skupina byla úspěšně odebrána.');
    }

    private function displayForm($request, $data = null)
    {
        $id = $request->getId() ?: '0';

        $groupsSelected = array_flip(
            array_map(
                function ($item) {
                    return $item['pg_id'];
                },
                DBSkupiny::getSingleWithGroups($id)
            )
        );

        $groups = array_map(
            function ($item) use ($groupsSelected) {
                return [
                    'buttons' => $this->checkbox('group[]', $item['pg_id'])
                                      ->set(isset($groupsSelected[$item['pg_id']]))
                                      ->render(),
                    'type' => ($item['pg_type'] == '1'
                               ? 'Členské příspěvky'
                               : 'Běžné platby'),
                    'name' => $item['pg_name'],
                    'base' => $item['pg_base']
                ];
            },
            DBPlatbyGroup::getGroups()
        );

        $action = $request->getAction();
        $this->render(
            'files/View/Admin/Skupiny/Form.inc',
            [
                'id' => $id,
                'name' => $request->post('name') ?: ($data ? $data['s_name'] : ''),
                'color' => $request->post('color') ?: ($data ? $data['s_color_rgb'] : ''),
                'popis' => $request->post('popis') ?: ($data ? $data['s_description'] : ''),
                'action' => $action,
                'header' => $action == 'add' ? 'Přidat skupinu' : 'Upravit skupinu',
                'groups' => $groups
            ]
        );
    }

    private function getLinkedSkupinaObjects($id)
    {
        $group = DBSkupiny::getSingleWithGroups($id);
        return $group ? ['groups' => $group] : [];
    }
    private function checkPost($request)
    {
        $f = new Form();

        $f->checkNotEmpty($request->post('name'), 'Zadejte prosím nějaké jméno.');
        $f->checkNotEmpty($request->post('desc'), 'Zadejte prosím nějaký popis.');
        $f->checkRegexp($request->post('color'), '/#[0-9a-f]{6}/i', 'Zadejte prosím platnou barvu.');

        return $f->isValid() ? true : $f;
    }
}
