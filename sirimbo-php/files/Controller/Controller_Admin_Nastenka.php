<?php
class Controller_Admin_Nastenka
{
    public function view($request)
    {
        \Permissions::checkError('nastenka', P_OWNED);
        $pager = new \Paging(new \DBNastenka());
        $pager->setCurrentPage($_GET['p']);
        $pager->setItemsPerPage($_GET['c']);
        $data = $pager->getItems();

        $showButtonsCol = false;
        $data = array_map(
            function ($item) use (&$showButtonsCol) {
                $canEdit = \Permissions::check('nastenka', P_OWNED, $item['up_kdo']);
                $buttons = '';
                if ($canEdit) {
                    $showButtonsCol = true;
                    $buttons = new \EditLinkHelper('/admin/nastenka/edit/' . $item['up_id'])
                        . '&nbsp;&nbsp;'
                        . new \RemoveLinkHelper('/admin/nastenka/remove/' . $item['up_id']);
                }
                return [
                    'buttons' => $buttons,
                    'header' => $item['up_nadpis'],
                    'fullName' => $item['u_jmeno'] . ' ' . $item['u_prijmeni'],
                    'timestampAdd' => formatTimestamp($item['up_timestamp_add'], true),
                    'groups' => array_reduce(
                        \DBNastenka::getNastenkaSkupiny($item['up_id']),
                        fn($carry, $item) => $carry . new ColorboxHelper($item['ups_color'], $item['ups_popis']),
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
            'navigation' => $pager->getNavigation()
        ]);
    }

    public static function renderForm($request)
    {
        $skupiny = \DBSkupiny::get();
        $skupinySelected = [];
        foreach ($skupiny as $item) {
            $skupinySelected[$item['s_id']] = $_POST['sk-' . $item['s_id']];
        }
        new \RenderHelper('files/View/Admin/Nastenka/Form.inc', [
            'header' => 'Správa nástěnky',
            'subheader' => ($request->getAction() === 'add') ? 'Přidat příspěvek' : 'Upravit příspěvek',
            'action' => $request->getAction(),
            'returnURI' => $_SERVER['HTTP_REFERER'] ?: '/admin/nastenka',
            'skupiny' => $skupiny,
            'skupinySelected' => $skupinySelected,
            'nadpis' => $_POST['nadpis'] ?: '',
            'text' => $_POST['text'] ?: '',
            'lock' => $_POST['lock'] ?: ''
        ]);
    }

    public function add($request)
    {
        \Permissions::checkError('nastenka', P_OWNED);
        if (!$_POST) {
            return static::renderForm($request);
        }
        $form = static::checkData($request);
        if (!$form->isValid()) {
            new \MessageHelper('warning', $form->getMessages());
            return static::renderForm($request);
        }

        $id = \DBNastenka::addNastenka(
            \Session::getUserID(),
            $_POST['nadpis'],
            $_POST['text'],
            $_POST['lock'] ? 1 : 0
        );

        $skupiny = \DBSkupiny::get();
        foreach ($skupiny as $skupina) {
            if (!$_POST['sk-' . $skupina['s_id']]) {
                continue;
            }
            \DBNastenka::addNastenkaSkupina(
                $id,
                $skupina['s_id'],
                $skupina['s_color_rgb'],
                $skupina['s_description']
            );
        }

        new \RedirectHelper($_POST['returnURI']);
    }

    public function edit($request)
    {
        \Permissions::checkError('nastenka', P_OWNED);
        if (!$id = $request->getId()) {
            new \MessageHelper('warning', 'Nástěnka s takovým ID neexistuje');
            new \RedirectHelper($_POST['returnURI'] ?: '/admin/nastenka');
        }
        if (!$data = \DBNastenka::getSingleNastenka($id)) {
            new \MessageHelper('warning', 'Nástěnka s takovým ID neexistuje');
            new \RedirectHelper($_POST['returnURI'] ?: '/admin/nastenka');
        }
        \Permissions::checkError('nastenka', P_OWNED, $data['up_kdo']);

        if (!$_POST) {
            $_POST['id'] = $id;
            $_POST['nadpis'] = $data['up_nadpis'];
            $_POST['text'] = $data['up_text'];
            foreach (\DBNastenka::getNastenkaSkupiny($id) as $skupina) {
                $_POST['sk-' . $skupina['ups_id_skupina']] = 1;
            }
            $_POST['lock'] = $data['up_lock'];
            return static::renderForm($request);
        }
        $form = static::checkData($request);
        if (!$form->isValid()) {
            new \MessageHelper('warning', $form->getMessages());
            return static::renderForm($request);
        }

        $skupiny_old = [];
        foreach (\DBNastenka::getNastenkaSkupiny($id) as $skupina) {
            $skupiny_old[$skupina['ups_id_skupina']] = $skupina['ups_id'];
        }

        $skupiny_new = [];
        foreach (\DBSkupiny::get() as $item) {
            if ($_POST['sk-' . $item['s_id']]) {
                $skupiny_new[$item['s_id']] = $item;
            }
        }

        $oldIds = array_keys($skupiny_old);
        $newIds = array_keys($skupiny_new);
        foreach (array_diff($oldIds, $newIds) as $removed) {
            \DBNastenka::removeNastenkaSkupina($skupiny_old[$removed]);
        }
        foreach (array_diff($newIds, $oldIds) as $added) {
            $skupinaData = $skupiny_new[$added];
            \DBNastenka::addNastenkaSkupina(
                $id,
                $skupinaData['s_id'],
                $skupinaData['s_color_rgb'],
                $skupinaData['s_description']
            );
        }

        \DBNastenka::editNastenka(
            $id,
            $_POST['nadpis'],
            $_POST['text'],
            ($_POST['lock'] == 'lock') ? 1 : 0
        );

        new \RedirectHelper($_POST['returnURI']);
    }

    public function remove($request)
    {
        \Permissions::checkError('nastenka', P_OWNED);
        if (!$id = $request->getId()) {
            new \MessageHelper('warning', 'Příspěvek s takovým ID neexistuje');
            new \RedirectHelper($_POST['returnURI'] ?: '/admin/nastenka');
        }
        if (!$data = \DBNastenka::getSingleNastenka($id)) {
            new \MessageHelper('warning', 'Příspěvek s takovým ID neexistuje');
            new \RedirectHelper($_POST['returnURI'] ?: '/admin/nastenka');
        }
        \Permissions::checkError('nastenka', P_OWNED, $data['up_kdo']);

        if (!$_POST || $_POST['action'] != 'confirm') {
            return new \RenderHelper('files/View/Admin/RemovePrompt.inc', [
                'header' => 'Správa nástěnky',
                'prompt' => 'Opravdu chcete odstranit příspěvek:',
                'returnURI' => $_SERVER['HTTP_REFERER'] ?: '/admin/nastenka',
                'data' => [['id' => $data['up_id'], 'text' => $data['up_nadpis']]]
            ]);
        }

        \DBNastenka::removeNastenka($id);
        new \RedirectHelper('/admin/nastenka');
    }

    private static function checkData($request): \Form
    {
        $f = new \Form();
        $f->checkNotEmpty($_POST['nadpis'], 'Zadejte nadpis', 'nadpis');
        $f->checkNotEmpty($_POST['text'], 'Zadejte nějaký text', 'text');
        return $f;
    }
}
