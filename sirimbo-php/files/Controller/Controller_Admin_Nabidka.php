<?php
class Controller_Admin_Nabidka
{
    public function view($request)
    {
        \Permissions::checkError('nabidka', P_OWNED);
        $data = \Permissions::check('nabidka', P_ADMIN)
            ? \DBNabidka::getNabidka(true)
            : \DBNabidka::getNabidkyByTrener(Session::getUserID(), true);

        if ($_POST['action'] == 'save') {
            foreach ($data as $item) {
                if ((bool) $_POST[$item['n_id']] == (bool) $item['n_visible']) {
                    continue;
                }
                \DBNabidka::editNabidka(
                    $item['n_id'],
                    $item['n_trener'],
                    $item['n_pocet_hod'],
                    $item['n_max_pocet_hod'],
                    $item['n_od'],
                    $item['n_do'],
                    $_POST[$item['n_id']] ? '1' : '0',
                    $item['n_lock']
                );
            }
            new \RedirectHelper('/admin/nabidka');
        }

        $data = array_map(
            function ($item) {
                return [
                    'fullName' => $item['u_jmeno'] . ' ' . $item['u_prijmeni'],
                    'date' => (
                        formatDate($item['n_od']) .
                        ($item['n_do'] != $item['n_od']
                         ? ' - ' . formatDate($item['n_do'])
                         : '')
                    ),
                    'buttons' => new DuplicateLinkHelper('/admin/nabidka/duplicate/' . $item['n_id'])
                        . '&nbsp;' . new RemoveLinkHelper('/admin/nabidka/remove/' . $item['n_id']),
                    'links' => (
                        '<a href="/admin/nabidka/edit/' . $item['n_id'] . '">obecné</a>, ' .
                        '<a href="/admin/nabidka/detail/' . $item['n_id'] . '">tréninky</a>'
                    ),
                    'visible' => new \CheckboxHelper($item['n_id'], '1', $item['n_visible'])
                ];
            },
            $data
        );
        new \RenderHelper('files/View/Admin/Nabidka/Overview.inc', [
            'header' => 'Správa nabídky',
            'data' => $data
        ]);
    }

    public function add($request)
    {
        \Permissions::checkError('nabidka', P_OWNED);
        if (!$_POST) {
            return $this->displayForm($request);
        }
        $form = $this->checkData($request);
        if (!$form->isValid()) {
            new \MessageHelper('warning', $form->getMessages());
            return $this->displayForm($request);
        }

        \Permissions::checkError('nabidka', P_OWNED, $_POST['trener']);

        $od = new \Date($_POST['od'] ?? null);
        $do = new \Date($_POST['do'] ?? null);
        if (!$do->isValid() || strcmp((string) $od, (string) $do) > 0) {
            $do = $od;
        }

        if (!is_numeric($_POST['max_pocet_hod'])) {
            $_POST['max_pocet_hod'] = 0;
        }

        \DBNabidka::addNabidka(
            $_POST['trener'],
            $_POST['pocet_hod'],
            $_POST['max_pocet_hod'],
            (string) $od,
            (string) $do,
            $_POST['visible'] ? '1' : '0',
            $_POST['lock'] ? 1 : 0
        );

        new \MessageHelper('success', 'Nabídka přidána');
        new \RedirectHelper($_POST['returnURI'] ?: '/admin/nabidka');
    }

    public function edit($request)
    {
        \Permissions::checkError('nabidka', P_OWNED);
        if (!$id = $request->getId()) {
            new \MessageHelper('warning', 'Nabídka s takovým ID neexistuje');
            new \RedirectHelper($_POST['returnURI'] ?: '/admin/nabidka');
        }
        if (!$data = \DBNabidka::getSingleNabidka($id)) {
            new \MessageHelper('warning', 'Nabídka s takovým ID neexistuje');
            new \RedirectHelper($_POST['returnURI'] ?: '/admin/nabidka');
        }
        \Permissions::checkError('nabidka', P_OWNED, $data['n_trener']);

        if (!$_POST) {
            return $this->displayForm($request, $data);
        }
        $form = $this->checkData($request);
        if (!$form->isValid()) {
            new \MessageHelper('warning', $form->getMessages());
            return $this->displayForm($request, $data);
        }

        $od = new \Date($_POST['od'] ?? null);
        $do = new \Date($_POST['do'] ?? null);
        if (!$do->isValid() || strcmp((string) $od, (string) $do) > 0) {
            $do = $od;
        }

        $items = \DBNabidka::getNabidkaItemLessons($id);
        $pocet_hod = $_POST['pocet_hod'];
        if ($pocet_hod < $items) {
            $pocet_hod = $items;
            new \MessageHelper('warning', 
                'Obsazených hodin už je víc než jste zadali, ' .
                'nelze už dál snížit počet hodin'
            );
        }
        $max_lessons = $_POST['max_pocet_hod'];
        $max_lessons_old = \DBNabidka::getNabidkaMaxItems($id);
        if ($max_lessons < $max_lessons_old && $max_lessons != 0) {
            $max_lessons = $max_lessons_old;
            new \MessageHelper('warning', 
                'Zadaný maximální počet hodin/pár je méně než už je zarezervováno, ' .
                'nelze už dál snížit maximální počet hodin'
            );
        }
        if (!is_numeric($max_lessons)) {
            $max_lessons = 0;
        }

        \DBNabidka::editNabidka(
            $id,
            $_POST['trener'],
            $pocet_hod,
            $max_lessons,
            (string) $od,
            (string) $do,
            $_POST['visible'] ? '1' : '0',
            $_POST['lock'] ? '1' : '0'
        );

        new \MessageHelper('success', 'Nabídka úspěšně upravena');
        new \RedirectHelper($_POST['returnURI'] ?: '/admin/nabidka');
    }

    public function duplicate($request)
    {
        \Permissions::checkError('nabidka', P_OWNED);
        $oldId = $request->getId();
        $data = \DBNabidka::getSingleNabidka($oldId);
        $items = \DBNabidka::getNabidkaItem($oldId);

        $newId = \DBNabidka::addNabidka(
            $data['n_trener'],
            $data['n_pocet_hod'],
            $data['n_max_pocet_hod'],
            $data['n_od'],
            $data['n_do'],
            $data['n_visible'],
            $data['n_lock']
        );
        foreach ($items as $item) {
            \DBNabidka::addNabidkaItemLessons(
                $item['ni_partner'],
                $newId,
                $item['ni_pocet_hod']
            );
        }
        new \RedirectHelper('/admin/nabidka');
    }

    public function remove($request)
    {
        \Permissions::checkError('nabidka', P_OWNED);
        $id = $request->getId();
        $data = \DBNabidka::getSingleNabidka($id);
        if (!\Permissions::check('nabidka', P_OWNED, $data['n_trener'])) {
            throw new AuthorizationException("Máte nedostatečnou autorizaci pro tuto akci!");
        }
        \DBNabidka::removeNabidka($id);
        new \RedirectHelper('/admin/nabidka');
    }

    protected function displayForm($request, $data = null)
    {
        $isAdmin = \Permissions::check('nabidka', P_ADMIN);
        if ($isAdmin) {
            $treneri = \DBUser::getUsersByPermission('nabidka', P_OWNED);
        } else {
            $treneri = [\DBUser::getUserData(Session::getUserID())];
        }
        new \RenderHelper('files/View/Admin/Nabidka/Form.inc', [
            'header' => 'Správa nabídky',
            'subheader' => $request->getAction() == 'add' ? 'Přidat nabídku' : 'Upravit nabídku',
            'action' => $request->getAction() == 'add' ? 'Přidat' : 'Upravit',
            'returnURI' => $request->getReferer(),
            'users' => $treneri,
            'id' => $data ? $data['n_id'] : null,
            'trener' => $_POST['trener'] ?: ($data ? $data['n_trener'] : ''),
            'pocet_hod' => $_POST['pocet_hod'] ?: ($data ? $data['n_pocet_hod'] : ''),
            'max_pocet_hod' => $_POST['max_pocet_hod'] ?: ($data ? $data['n_max_pocet_hod'] : ''),
            'od' => $_POST['od'] ?: ($data ? $data['n_od'] : ''),
            'do' => $_POST['do'] ?: ($data ? $data['n_do'] : ''),
            'visible' => $_POST['visible'] ?: ($data ? $data['n_visible'] : false),
            'lock' => $_POST['lock'] ?: ($data ? $data['n_lock'] : '')
        ]);
    }

    private function checkData($request): Form
    {
        $od = new \Date($_POST['od'] ?? null);
        $do = new \Date($_POST['do'] ?? null);
        if (!$do->isValid() || strcmp((string) $od, (string) $do) > 0) {
            $do = $od;
        }

        $f = new Form();
        $f->checkNumeric($_POST['trener'], 'ID trenéra musí být číselné', 'trener');
        $f->checkNumeric($_POST['pocet_hod'], 'Počet hodin prosím zadejte čísly', 'pocet_hod');
        $f->checkDate((string) $od, 'Zadejte prosím platné datum ("Od")', 'od');
        if ($do->isValid()) {
            $f->checkDate((string) $do, 'Zadejte prosím platné datum ("Do")', 'do');
        }

        return $f;
    }
}
