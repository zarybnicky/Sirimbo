<?php
class Controller_Admin_Nabidka extends Controller_Abstract
{
    public function __construct($request)
    {
        parent::__construct($request);
        Permissions::checkError('nabidka', P_OWNED);
    }

    public function view($request)
    {
        $data = Permissions::check('nabidka', P_ADMIN)
            ? DBNabidka::getNabidka(true)
            : DBNabidka::getNabidkyByTrener(Session::getUserID(), true);

        if ($request->post('action') == 'save') {
            foreach ($data as $item) {
                if ((bool) $request->post($item['n_id']) == (bool) $item['n_visible']) {
                    continue;
                }
                DBNabidka::editNabidka(
                    $item['n_id'],
                    $item['n_trener'],
                    $item['n_pocet_hod'],
                    $item['n_max_pocet_hod'],
                    $item['n_od'],
                    $item['n_do'],
                    $request->post($item['n_id']) ? '1' : '0',
                    $item['n_lock']
                );
            }
            $this->redirect('/admin/nabidka');
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
                    'visible' => (string) $this->checkbox($item['n_id'], '1')->set($item['n_visible'])
                ];
            },
            $data
        );
        $this->render('files/View/Admin/Nabidka/Overview.inc', [
            'header' => 'Správa nabídky',
            'data' => $data
        ]);
    }

    public function add($request)
    {
        if (!$request->post()) {
            $this->displayForm($request);
            return;
        }
        $form = $this->checkData($request);
        if (!$form->isValid()) {
            $this->redirect()->warning($form->getMessages());
            $this->displayForm($request);
            return;
        }

        Permissions::checkError('nabidka', P_OWNED, $request->post('trener'));

        $od = $this->date('od')->getPost($request);
        $do = $this->date('do')->getPost($request);
        if (!$do->isValid() || strcmp((string) $od, (string) $do) > 0) {
            $do = $od;
        }

        if (!is_numeric($request->post('max_pocet_hod'))) {
            $request->post('max_pocet_hod', 0);
        }

        DBNabidka::addNabidka(
            $request->post('trener'),
            $request->post('pocet_hod'),
            $request->post('max_pocet_hod'),
            (string) $od,
            (string) $do,
            $request->post('visible') ? '1' : '0',
            $request->post('lock') ? 1 : 0
        );

        $this->redirect()->success('Nabídka přidána');
        $this->redirect($request->post('returnURI') ?: '/admin/nabidka');
    }

    public function edit($request)
    {
        if (!$id = $request->getId()) {
            $this->redirect()->warning('Nabídka s takovým ID neexistuje');
            $this->redirect($request->post('returnURI') ?: '/admin/nabidka');
        }
        if (!$data = DBNabidka::getSingleNabidka($id)) {
            $this->redirect()->warning('Nabídka s takovým ID neexistuje');
            $this->redirect($request->post('returnURI') ?: '/admin/nabidka');
        }
        Permissions::checkError('nabidka', P_OWNED, $data['n_trener']);

        if (!$request->post()) {
            $this->displayForm($request, $data);
            return;
        }
        $form = $this->checkData($request);
        if (!$form->isValid()) {
            $this->redirect()->warning($form->getMessages());
            $this->displayForm($request, $data);
            return;
        }

        $od = $this->date('od')->getPost($request);
        $do = $this->date('do')->getPost($request);
        if (!$do->isValid() || strcmp((string) $od, (string) $do) > 0) {
            $do = $od;
        }

        $items = DBNabidka::getNabidkaItemLessons($id);
        $pocet_hod = $request->post('pocet_hod');
        if ($pocet_hod < $items) {
            $pocet_hod = $items;
            $this->redirect()->warning(
                'Obsazených hodin už je víc než jste zadali, ' .
                'nelze už dál snížit počet hodin'
            );
        }
        $max_lessons = $request->post('max_pocet_hod');
        $max_lessons_old = DBNabidka::getNabidkaMaxItems($id);
        if ($max_lessons < $max_lessons_old && $max_lessons != 0) {
            $max_lessons = $max_lessons_old;
            $this->redirect()->warning(
                'Zadaný maximální počet hodin/pár je méně než už je zarezervováno, ' .
                'nelze už dál snížit maximální počet hodin'
            );
        }
        if (!is_numeric($max_lessons)) {
            $max_lessons = 0;
        }

        DBNabidka::editNabidka(
            $id,
            $request->post('trener'),
            $pocet_hod,
            $max_lessons,
            (string) $od,
            (string) $do,
            $request->post('visible') ? '1' : '0',
            $request->post('lock') ? '1' : '0'
        );

        $this->redirect()->success('Nabídka úspěšně upravena');
        $this->redirect($request->post('returnURI') ?: '/admin/nabidka');
    }

    public function duplicate($request)
    {
        $oldId = $request->getId();
        $data = DBNabidka::getSingleNabidka($oldId);
        $items = DBNabidka::getNabidkaItem($oldId);

        $newId = DBNabidka::addNabidka(
            $data['n_trener'],
            $data['n_pocet_hod'],
            $data['n_max_pocet_hod'],
            $data['n_od'],
            $data['n_do'],
            $data['n_visible'],
            $data['n_lock']
        );
        foreach ($items as $item) {
            DBNabidka::addNabidkaItemLessons(
                $item['ni_partner'],
                $newId,
                $item['ni_pocet_hod']
            );
        }
        $this->redirect('/admin/nabidka');
    }

    public function remove($request)
    {
        $id = $request->getId();
        $data = DBNabidka::getSingleNabidka($id);
        if (!Permissions::check('nabidka', P_OWNED, $data['n_trener'])) {
            throw new AuthorizationException("Máte nedostatečnou autorizaci pro tuto akci!");
        }
        DBNabidka::removeNabidka($id);
        $this->redirect('/admin/nabidka');
    }

    protected function displayForm($request, $data = null)
    {
        $isAdmin = Permissions::check('nabidka', P_ADMIN);
        if ($isAdmin) {
            $treneri = DBUser::getUsersByPermission('nabidka', P_OWNED);
        } else {
            $treneri = [DBUser::getUserData(Session::getUserID())];
        }
        $this->render('files/View/Admin/Nabidka/Form.inc', [
            'header' => 'Správa nabídky',
            'subheader' => $request->getAction() == 'add' ? 'Přidat nabídku' : 'Upravit nabídku',
            'action' => $request->getAction() == 'add' ? 'Přidat' : 'Upravit',
            'returnURI' => $request->getReferer(),
            'users' => $treneri,
            'id' => $data ? $data['n_id'] : null,
            'trener' => $request->post('trener') ?: ($data ? $data['n_trener'] : ''),
            'pocet_hod' => $request->post('pocet_hod') ?: ($data ? $data['n_pocet_hod'] : ''),
            'max_pocet_hod' => $request->post('max_pocet_hod') ?: ($data ? $data['n_max_pocet_hod'] : ''),
            'od' => $request->post('od') ?: ($data ? $data['n_od'] : ''),
            'do' => $request->post('do') ?: ($data ? $data['n_do'] : ''),
            'visible' => $request->post('visible') ?: ($data ? $data['n_visible'] : false),
            'lock' => $request->post('lock') ?: ($data ? $data['n_lock'] : '')
        ]);
        return;
    }

    private function checkData($request): Form
    {
        $od = $this->date('od')->getPost($request);
        $do = $this->date('do')->getPost($request);
        if (!$do->isValid() || strcmp((string) $od, (string) $do) > 0) {
            $do = $od;
        }

        $f = new Form();
        $f->checkNumeric($request->post('trener'), 'ID trenéra musí být číselné', 'trener');
        $f->checkNumeric($request->post('pocet_hod'), 'Počet hodin prosím zadejte čísly', 'pocet_hod');
        $f->checkDate((string) $od, 'Zadejte prosím platné datum ("Od")', 'od');
        if ($do->isValid()) {
            $f->checkDate((string) $do, 'Zadejte prosím platné datum ("Do")', 'do');
        }

        return $f;
    }
}
