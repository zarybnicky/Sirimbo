<?php
require_once 'files/Controller/Admin.php';
class Controller_Admin_Nastenka extends Controller_Admin
{
    public function __construct() {
        Permissions::checkError('nastenka', P_OWNED);
    }
    public function view($request) {
        switch($request->post('action')) {
        case 'remove':
            if (!is_array($request->post('nastenka'))) {
                break;
            }
            foreach ($request->post('nastenka') as $item) {
                $data = DBNastenka::getSingleNastenka($item);
                if (!Permissions::check('nastenka', P_OWNED, $data['up_kdo'])) {
                    $error = true;
                    continue;
                }
                DBNastenka::removeNastenka($item);
            }
            if (isset($error) && $error) {
                throw new AuthorizationException("Máte nedostatečnou autorizaci pro tuto akci!");
            }
            $this->redirect()->setMessage('Příspěvky odebrány');
            break;
        case 'edit':
            $nastenka = $request->post('nastenka');
            if ($nastenka[0]) {
                $this->redirect('/admin/nastenka/edit/' . $nastenka[0]);
            }
            break;
        }
        $pager = new Paging(new PagingAdapterDBSelect('DBNastenka'));
        $pager->setCurrentPage($request->get('p'));
        $pager->setItemsPerPage($request->get('c'));
        $pager->setCurrentPageField('p');
        $pager->setItemsPerPageField('c');
        $pager->setDefaultItemsPerPage(20);
        $pager->setPageRange(5);
        $data = $pager->getItems();

        $data = array_map(
            function ($item) {
                $canEdit = Permissions::check('nastenka', P_OWNED, $item['up_kdo']);
                return array(
                    'canEdit' => $canEdit,
                    'checkBox' => $canEdit ? $this->checkbox('nastenka[]', $item['up_id']) : '&nbsp;&#10799;',
                    'header' => $item['up_nadpis'],
                    'fullName' => $item['u_jmeno'] . ' ' . $item['u_prijmeni'],
                    'timestampAdd' => formatTimestamp($item['up_timestamp_add'], true),
                    'timestampEdit' => formatTimestamp($item['up_timestamp'], true),
                    'groups' => array_reduce(
                        DBNastenka::getNastenkaSkupiny($item['up_id']),
                        function ($carry, $item) {
                            return $carry . $this->colorbox($item['ups_color'], $item['ups_popis']);
                        },
                        ''
                    )
                );
            },
            $data
        );

        $this->render(
            'files/View/Admin/Nastenka/Overview.inc',
            array(
                'showMenu' => !TISK,
                'data' => $data,
                'navigation' => $pager->getNavigation($request->get())
            )
        );
    }
    public function add($request) {
        if (!$request->post() || is_object($f = $this->checkData($request))) {
            if ($request->post()) {
                $this->redirect()->setMessage($f->getMessages());
            }
            $skupiny = DBSkupiny::get();
            $skupinySelected = array();
            foreach ($skupiny as $item) {
                $skupinySelected[$item['s_id']] = $request->post('sk-' . $item['s_id']);
            }
            $this->render(
                'files/View/Admin/Nastenka/Form.inc',
                array(
                    'action' => $request->getAction(),
                    'referer' => $request->getReferer(),
                    'returnURI' => $request->getReferer(),
                    'skupiny' => $skupiny,
                    'skupinySelected' => $skupinySelected,
                    'nadpis' => $request->post('nadpis') ?: '',
                    'text' => $request->post('text') ?: '',
                    'lock' => $request->post('lock') ?: ''
                )
            );
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

        $this->redirect(
            $request->post('referer') ?: '/admin/nastenka',
            'Příspěvek úspěšně přidán'
        );
    }

    public function edit($request) {
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
                $request->post('text', stripslashes($data['up_text']));

                foreach ($skupiny as $skupina) {
                    $request->post('sk-' . $skupina['ups_id_skupina'], 1);
                }
                $request->post('lock', $data['up_lock']);
            } else {
                $this->redirect()->setMessage($f->getMessages());
            }
            $skupiny = DBSkupiny::get();
            $skupinySelected = array();
            foreach ($skupiny as $item) {
                $skupinySelected[$item['s_id']] = $request->post('sk-' . $item['s_id']);
            }

            $this->render(
                'files/View/Admin/Nastenka/Form.inc',
                array(
                    'action' => $request->getAction(),
                    'referer' => $request->getReferer(),
                    'returnURI' => $request->getReferer(),
                    'skupiny' => $skupiny,
                    'skupinySelected' => $skupinySelected,
                    'nadpis' => $request->post('nadpis') ?: '',
                    'text' => $request->post('text') ?: '',
                    'lock' => $request->post('lock') ?: ''
                )
            );
            return;
        }

        $skupiny_all = DBSkupiny::get();
        $skupiny_old = array();
        foreach (DBNastenka::getNastenkaSkupiny($id) as $skupina) {
            $skupiny_old[$skupina['ups_id_skupina']] = $skupina['ups_id'];
        }
        $skupiny_new = array();
        foreach ($skupiny_all as $item) {
            $skupiny_new[$item['s_id']] = (bool) $request->post('sk-' . $item['s_id']);
        }

        foreach ($skupiny_all as $skupina) {
            $groupOld = isset($skupiny_old[$skupina['s_id']]);
            $groupNew = $skupiny_new[$skupina['s_id']];

            if ($groupNew && !$groupOld) {
                DBNastenka::addNastenkaSkupina(
                    $id,
                    $skupina['s_id'],
                    $skupina['s_color_rgb'],
                    $skupina['s_description']
                );
            } elseif (!$groupNew && $groupOld) {
                DBNastenka::removeNastenkaSkupina($skupiny_old[$skupina['s_id']]);
            }
        }
        DBNastenka::editNastenka(
            $id,
            $request->post('nadpis'),
            $request->post('text'),
            ($request->post('lock') == 'lock') ? 1 : 0
        );

        $this->redirect(
            $request->post('referer') ?: '/admin/nastenka',
            'Příspěvek úspěšně upraven'
        );
    }

    private function checkData($request) {
        $f = new Form();
        $f->checkNotEmpty($request->post('nadpis'), 'Zadejte nadpis', 'nadpis');
        $f->checkNotEmpty($request->post('text'), 'Zadejte nějaký text', 'text');
        return $f->isValid() ? true : $f;
    }
}
