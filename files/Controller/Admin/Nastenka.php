<?php
require_once 'files/Controller/Admin.php';
class Controller_Admin_Nastenka extends Controller_Admin
{
    public function __construct()
    {
        Permissions::checkError('nastenka', P_OWNED);
    }

    public function view($request)
    {
        $pager = new Paging(new PagingAdapterDBSelect('DBNastenka'));
        $pager->setCurrentPage($request->get('p'));
        $pager->setItemsPerPage($request->get('c'));
        $pager->setCurrentPageField('p');
        $pager->setItemsPerPageField('c');
        $pager->setDefaultItemsPerPage(20);
        $pager->setPageRange(5);
        $data = $pager->getItems();

        $showButtonsCol = false;
        $data = array_map(
            function ($item) use (&$showButtonsCol) {
                $canEdit = Permissions::check('nastenka', P_OWNED, $item['up_kdo']);
                $buttons = '';
                if ($canEdit) {
                    $showButtonsCol = true;
                    $buttons = $this->editLink('/admin/nastenka/edit/' . $item['up_id'])
                        . '&nbsp;&nbsp;'
                        . $this->removeLink('/admin/nastenka/remove/' . $item['up_id']);
                }
                return [
                    'buttons' => $buttons,
                    'header' => $item['up_nadpis'],
                    'fullName' => $item['u_jmeno'] . ' ' . $item['u_prijmeni'],
                    'timestampAdd' => formatTimestamp($item['up_timestamp_add'], true),
                    'groups' => array_reduce(
                        DBNastenka::getNastenkaSkupiny($item['up_id']),
                        function ($carry, $item) {
                            return $carry . $this->colorbox($item['ups_color'], $item['ups_popis']);
                        },
                        ''
                    )
                ];
            },
            $data
        );

        $this->render('files/View/Admin/Nastenka/Overview.inc', [
            'header' => 'Správa nástěnky',
            'showButtonsCol' => $showButtonsCol,
            'data' => $data,
            'navigation' => $pager->getNavigation($request->get())
        ]);
    }

    public function add($request)
    {
        if (!$request->post() || is_object($f = $this->checkData($request))) {
            if ($request->post()) {
                $this->redirect()->setMessage($f->getMessages());
            }
            $skupiny = DBSkupiny::get();
            $skupinySelected = [];
            foreach ($skupiny as $item) {
                $skupinySelected[$item['s_id']] = $request->post('sk-' . $item['s_id']);
            }
            $this->render('files/View/Admin/Nastenka/Form.inc', [
                'header' => 'Správa nástěnky',
                'subheader' => ($this->action == 'add' ? 'Přidat' : 'Upravit') . ' příspěvek',
                'action' => $request->getAction(),
                'returnURI' => $request->getReferer() ?: '/admin/nastenka',
                'skupiny' => $skupiny,
                'skupinySelected' => $skupinySelected,
                'nadpis' => $request->post('nadpis') ?: '',
                'text' => $request->post('text') ?: '',
                'lock' => $request->post('lock') ?: ''
            ]);
            return;
        }
        $id = DBNastenka::addNastenka(
            User::getUserID(),
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

        $this->redirect($request->post('referer'), 'Příspěvek úspěšně přidán');
    }

    public function edit($request)
    {
        $id = $request->getId();
        if (!$id || !($data = DBNastenka::getSingleNastenka($id))) {
            $this->redirect(
                $request->post('referer') ?: '/admin/nastenka',
                'Nástěnka s takovým ID neexistuje'
            );
        }
        Permissions::checkError('nastenka', P_OWNED, $data['up_kdo']);

        if (!$request->post() || is_object($f = $this->checkData($request))) {
            $skupiny = DBNastenka::getNastenkaSkupiny($id);

            if (!$request->post()) {
                $request->post('id', $id);
                $request->post('nadpis', $data['up_nadpis']);
                $request->post('text', $data['up_text']);

                foreach ($skupiny as $skupina) {
                    $request->post('sk-' . $skupina['ups_id_skupina'], 1);
                }
                $request->post('lock', $data['up_lock']);
            } else {
                $this->redirect()->setMessage($f->getMessages());
            }
            $skupiny = DBSkupiny::get();
            $skupinySelected = [];
            foreach ($skupiny as $item) {
                $skupinySelected[$item['s_id']] = $request->post('sk-' . $item['s_id']);
            }

            $this->render('files/View/Admin/Nastenka/Form.inc', [
                'header' => 'Správa nástěnky',
                'subheader' => ($this->action == 'add' ? 'Přidat' : 'Upravit') . ' příspěvek',
                'action' => $request->getAction(),
                'returnURI' => $request->getReferer() ?: '/admin/nastenka',
                'skupiny' => $skupiny,
                'skupinySelected' => $skupinySelected,
                'nadpis' => $request->post('nadpis') ?: '',
                'text' => $request->post('text') ?: '',
                'lock' => $request->post('lock') ?: ''
            ]);
            return;
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

        $this->redirect($request->post('referer'), 'Příspěvek úspěšně upraven');
    }

    public function remove($request)
    {
        $id = $request->getId();
        if (!$id || !($data = DBNastenka::getSingleNastenka($id))) {
            $this->redirect(
                $request->post('referer') ?: '/admin/nastenka',
                'Příspěvek s takovým ID neexistuje'
            );
        }
        Permissions::checkError('nastenka', P_OWNED, $data['up_kdo']);

        if (!$request->post() || $request->post('action') != 'confirm') {
            $this->render('files/View/Admin/RemovePrompt.inc', [
                'header' => 'Správa nástěnky',
                'prompt' => 'Opravdu chcete odstranit příspěvek:',
                'returnURI' => $request->getReferer() ?: '/admin/nastenka',
                'data' => [['id' => $data['up_id'], 'text' => $data['up_nadpis']]]
            ]);
            return;
        }

        DBNastenka::removeNastenka($id);
        $this->redirect('/admin/nastenka', 'Příspěvek odebrán');
    }

    private function checkData($request)
    {
        $f = new Form();
        $f->checkNotEmpty($request->post('nadpis'), 'Zadejte nadpis', 'nadpis');
        $f->checkNotEmpty($request->post('text'), 'Zadejte nějaký text', 'text');
        return $f->isValid() ? true : $f;
    }
}
