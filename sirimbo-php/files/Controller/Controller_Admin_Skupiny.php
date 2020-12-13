<?php
class Controller_Admin_Skupiny extends Controller_Abstract
{
    public function __construct($request)
    {
        parent::__construct($request);
        Permissions::checkError('skupiny', P_OWNED);
    }

    public function view($request)
    {
        $data = array_map(
            function ($item) {
                return [
                    'buttons' => (
                        new EditLinkHelper('/admin/skupiny/edit/' . $item['s_id'])
                        . new RemoveLinkHelper('/admin/skupiny/remove/' . $item['s_id'])
                    ),
                    'colorBox' => new ColorboxHelper($item['s_color_rgb'], $item['s_description']),
                    'name' => $item['s_name']
                ];
            },
            DBSkupiny::get()
        );
        $this->render('files/View/Admin/Skupiny/Overview.inc', [
            'header' => 'Správa skupin',
            'data' => $data
        ]);
    }

    public function add($request)
    {
        if (!$request->post()) {
            return $this->displayForm($request);
        }
        $form = $this->checkData($request);
        if (!$form->isValid()) {
            $this->redirect()->warning($form->getMessages());
            return $this->displayForm($request);
        }
        DBSkupiny::insert(
            $request->post('name'),
            $request->post('color'),
            $request->post('desc')
        );
        $insertId = DBSkupiny::getInsertId();

        foreach ($request->post('group') ?: [] as $item) {
            DBSkupiny::addChild($insertId, $item);
        }

        $this->redirect('/admin/skupiny');
    }

    public function edit($request)
    {
        if (!$id = $request->getId()) {
            $this->redirect()->warning('Skupina s takovým ID neexistuje');
            $this->redirect('/admin/skupiny');
        }
        if (!$data = DBSkupiny::getSingle($id)) {
            $this->redirect()->warning('Skupina s takovým ID neexistuje');
            $this->redirect('/admin/skupiny');
        }

        if (!$request->post()) {
            return $this->displayForm($request, $data);
        }
        $form = $this->checkData($request);
        if (!$form->isValid()) {
            $this->redirect()->warning($form->getMessages());
            return $this->displayForm($request, $data);
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

        $this->redirect('/admin/skupiny');
    }

    public function remove($request)
    {
        if (!$id = $request->getId()) {
            $this->redirect()->warning('Skupina s takovým ID neexistuje');
            $this->redirect('/admin/skupiny');
        }
        if (!$data = DBSkupiny::getSingle($id)) {
            $this->redirect()->warning('Skupina s takovým ID neexistuje');
            $this->redirect('/admin/skupiny');
        }

        if ($request->post('action') == 'unlink') {
            $f = $this->getLinkedSkupinaObjects($id);

            $groupCount = 0;
            foreach ($f['groups'] as $data) {
                DBSkupiny::removeChild($id, $data['pg_id']);
                ++$groupCount;
            }

            $this->redirect()->info('Spojení s ' . $groupCount . ' kategoriemi byla odstraněna.');
            $this->redirect('/admin/skupiny/remove/' . $id);
            return;
        }
        if (($f = $this->getLinkedSkupinaObjects($id)) || !$request->post()) {
            if (isset($f) && $f) {
                $this->redirect()->info(
                    'Nemůžu odstranit skupinu s připojenými kategoriemi! '
                    . new Tag(
                        'form',
                        ['action' => '', 'mthod' => 'post'],
                        $this->submit('Odstranit spojení?')->data('action', 'unlink')
                    )
                );
            }
            $this->render('files/View/Admin/RemovePrompt.inc', [
                'header' => 'Správa skupin',
                'prompt' => 'Opravdu chcete odstranit skupinu?',
                'returnURI' => $request->getReferer() ?: '/admin/skupiny',
                'data' => [['id' => $data['s_id'], 'text' => $data['s_name']]]
            ]);
            return;
        }
        DBSkupiny::delete($id);
        $this->redirect('/admin/skupiny');
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
                                      ->set(isset($groupsSelected[$item['pg_id']])),
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
        $this->render('files/View/Admin/Skupiny/Form.inc', [
            'header' => 'Správa skupin',
            'subheader' => $action == 'add' ? 'Přidat skupinu' : 'Upravit skupinu',
            'id' => $id,
            'name' => $request->post('name') ?: ($data ? $data['s_name'] : ''),
            'color' => $request->post('color') ?: ($data ? $data['s_color_rgb'] : ''),
            'popis' => $request->post('popis') ?: ($data ? $data['s_description'] : ''),
            'action' => $action,
            'groups' => $groups
        ]);
    }

    private function getLinkedSkupinaObjects($id)
    {
        $group = DBSkupiny::getSingleWithGroups($id);
        return $group ? ['groups' => $group] : [];
    }

    private function checkData($request): Form
    {
        $f = new Form();
        $f->checkNotEmpty($request->post('name'), 'Zadejte prosím nějaké jméno.');
        $f->checkNotEmpty($request->post('desc'), 'Zadejte prosím nějaký popis.');
        $f->checkRegexp($request->post('color'), '/#[0-9a-f]{6}/i', 'Zadejte prosím platnou barvu.');
        return $f;
    }
}
