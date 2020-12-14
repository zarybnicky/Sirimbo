<?php
class Controller_Admin_Nastenka
{
    public function view($request)
    {
        Permissions::checkError('nastenka', P_OWNED);
        $pager = new Paging(new DBNastenka());
        $pager->setCurrentPage($request->get('p'));
        $pager->setItemsPerPage($request->get('c'));
        $data = $pager->getItems();

        $showButtonsCol = false;
        $data = array_map(
            function ($item) use (&$showButtonsCol) {
                $canEdit = Permissions::check('nastenka', P_OWNED, $item['up_kdo']);
                $buttons = '';
                if ($canEdit) {
                    $showButtonsCol = true;
                    $buttons = new EditLinkHelper('/admin/nastenka/edit/' . $item['up_id'])
                        . '&nbsp;&nbsp;'
                        . new RemoveLinkHelper('/admin/nastenka/remove/' . $item['up_id']);
                }
                return [
                    'buttons' => $buttons,
                    'header' => $item['up_nadpis'],
                    'fullName' => $item['u_jmeno'] . ' ' . $item['u_prijmeni'],
                    'timestampAdd' => formatTimestamp($item['up_timestamp_add'], true),
                    'groups' => array_reduce(
                        DBNastenka::getNastenkaSkupiny($item['up_id']),
                        function ($carry, $item) {
                            return $carry . new ColorboxHelper($item['ups_color'], $item['ups_popis']);
                        },
                        ''
                    )
                ];
            },
            $data
        );

        new \RenderHelper('files/View/Admin/Nastenka/Overview.inc', [
            'header' => 'Správa nástěnky',
            'showButtonsCol' => $showButtonsCol,
            'data' => $data,
            'navigation' => $pager->getNavigation($request->get())
        ]);
    }

    public function renderForm($request)
    {
        $skupiny = DBSkupiny::get();
        $skupinySelected = [];
        foreach ($skupiny as $item) {
            $skupinySelected[$item['s_id']] = $request->post('sk-' . $item['s_id']);
        }
        new \RenderHelper('files/View/Admin/Nastenka/Form.inc', [
            'header' => 'Správa nástěnky',
            'subheader' => ($request->getAction() === 'add') ? 'Přidat příspěvek' : 'Upravit příspěvek',
            'action' => $request->getAction(),
            'returnURI' => $request->getReferer() ?: '/admin/nastenka',
            'skupiny' => $skupiny,
            'skupinySelected' => $skupinySelected,
            'nadpis' => $request->post('nadpis') ?: '',
            'text' => $request->post('text') ?: '',
            'lock' => $request->post('lock') ?: ''
        ]);
    }

    public function add($request)
    {
        Permissions::checkError('nastenka', P_OWNED);
        if (!$request->post()) {
            return $this->renderForm($request);
        }
        $form = $this->checkData($request);
        if (!$form->isValid()) {
            new \MessageHelper('warning', $form->getMessages());
            return $this->renderForm($request);
        }

        $id = DBNastenka::addNastenka(
            Session::getUserID(),
            $request->post('nadpis'),
            $request->post('text'),
            $request->post('lock') ? 1 : 0
        );

        $skupiny = DBSkupiny::get();
        foreach ($skupiny as $skupina) {
            if (!$request->post('sk-' . $skupina['s_id'])) {
                continue;
            }
            DBNastenka::addNastenkaSkupina(
                $id,
                $skupina['s_id'],
                $skupina['s_color_rgb'],
                $skupina['s_description']
            );
        }

        new \RedirectHelper($request->post('returnURI'));
    }

    public function edit($request)
    {
        Permissions::checkError('nastenka', P_OWNED);
        if (!$id = $request->getId()) {
            new \MessageHelper('warning', 'Nástěnka s takovým ID neexistuje');
            new \RedirectHelper($request->post('returnURI') ?: '/admin/nastenka');
        }
        if (!$data = DBNastenka::getSingleNastenka($id)) {
            new \MessageHelper('warning', 'Nástěnka s takovým ID neexistuje');
            new \RedirectHelper($request->post('returnURI') ?: '/admin/nastenka');
        }
        Permissions::checkError('nastenka', P_OWNED, $data['up_kdo']);

        if (!$request->post()) {
            $request->post('id', $id);
            $request->post('nadpis', $data['up_nadpis']);
            $request->post('text', $data['up_text']);
            foreach (DBNastenka::getNastenkaSkupiny($id) as $skupina) {
                $request->post('sk-' . $skupina['ups_id_skupina'], 1);
            }
            $request->post('lock', $data['up_lock']);
            return $this->renderForm($request);
        }
        $form = $this->checkData($request);
        if (!$form->isValid()) {
            new \MessageHelper('warning', $form->getMessages());
            return $this->renderForm($request);
        }

        $skupiny_old = [];
        foreach (DBNastenka::getNastenkaSkupiny($id) as $skupina) {
            $skupiny_old[$skupina['ups_id_skupina']] = $skupina['ups_id'];
        }

        $skupiny_new = [];
        foreach (DBSkupiny::get() as $item) {
            if ($request->post('sk-' . $item['s_id'])) {
                $skupiny_new[$item['s_id']] = $item;
            }
        }

        $oldIds = array_keys($skupiny_old);
        $newIds = array_keys($skupiny_new);
        foreach (array_diff($oldIds, $newIds) as $removed) {
            DBNastenka::removeNastenkaSkupina($skupiny_old[$removed]);
        }
        foreach (array_diff($newIds, $oldIds) as $added) {
            $skupinaData = $skupiny_new[$added];
            DBNastenka::addNastenkaSkupina(
                $id,
                $skupinaData['s_id'],
                $skupinaData['s_color_rgb'],
                $skupinaData['s_description']
            );
        }

        DBNastenka::editNastenka(
            $id,
            $request->post('nadpis'),
            $request->post('text'),
            ($request->post('lock') == 'lock') ? 1 : 0
        );

        new \RedirectHelper($request->post('returnURI'));
    }

    public function remove($request)
    {
        Permissions::checkError('nastenka', P_OWNED);
        if (!$id = $request->getId()) {
            new \MessageHelper('warning', 'Příspěvek s takovým ID neexistuje');
            new \RedirectHelper($request->post('returnURI') ?: '/admin/nastenka');
        }
        if (!$data = DBNastenka::getSingleNastenka($id)) {
            new \MessageHelper('warning', 'Příspěvek s takovým ID neexistuje');
            new \RedirectHelper($request->post('returnURI') ?: '/admin/nastenka');
        }
        Permissions::checkError('nastenka', P_OWNED, $data['up_kdo']);

        if (!$request->post() || $request->post('action') != 'confirm') {
            return new \RenderHelper('files/View/Admin/RemovePrompt.inc', [
                'header' => 'Správa nástěnky',
                'prompt' => 'Opravdu chcete odstranit příspěvek:',
                'returnURI' => $request->getReferer() ?: '/admin/nastenka',
                'data' => [['id' => $data['up_id'], 'text' => $data['up_nadpis']]]
            ]);
        }

        DBNastenka::removeNastenka($id);
        new \RedirectHelper('/admin/nastenka');
    }

    private function checkData($request): Form
    {
        $f = new Form();
        $f->checkNotEmpty($request->post('nadpis'), 'Zadejte nadpis', 'nadpis');
        $f->checkNotEmpty($request->post('text'), 'Zadejte nějaký text', 'text');
        return $f;
    }
}
