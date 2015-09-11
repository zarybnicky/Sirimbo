<?php
require_once 'files/Controller/Admin.php';
class Controller_Admin_Nabidka extends Controller_Admin
{
    public function __construct()
    {
        Permissions::checkError('nabidka', P_OWNED);
    }

    public function view($request)
    {
        switch($request->post('action')) {
        case 'save':
            foreach (DBNabidka::getNabidka() as $item) {
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
            $this->redirect('/admin/nabidka', 'Nabídky upraveny');
            break;

        case 'remove':
            if (!$request->post('nabidka')) {
                break;
            }
            foreach ($request->post('nabidka') as $item) {
                $data = DBNabidka::getSingleNabidka($item);
                if (!Permissions::check('nabidka', P_OWNED, $data['n_trener'])) {
                    $error = true;
                    continue;
                }
                DBNabidka::removeNabidka($item);
            }
            if (isset($error) && $error) {
                throw new AuthorizationException("Máte nedostatečnou autorizaci pro tuto akci!");
            }
            $this->redirect('/admin/nabidka', 'Nabídky odebrány');
            break;

        case 'duplicate':
            foreach ($request->post('nabidka') as $oldId) {
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
            }
            $this->redirect('/admin/nabidka');
            break;
        }

        $data = array_map(
            function ($item) {
                $isTrainer = Permissions::check('nabidka', P_OWNED, $item['n_trener']);
                $isAdmin = Permissions::check('nabidka', P_ADMIN);

                return array(
                    'canEdit' => $isTrainer,
                    'fullName' => $item['u_jmeno'] . ' ' . $item['u_prijmeni'],
                    'date' => (
                        formatDate($item['n_od']) .
                        ($item['n_do'] != $item['n_od']
                         ? ' - ' . formatDate($item['n_do'])
                         : '')
                    ),
                    'links' => (
                        '<a href="/admin/nabidka/edit/' . $item['n_id'] . '">obecné</a>, ' .
                        '<a href="/admin/nabidka/detail/' . $item['n_id'] . '">tréninky</a>'
                    ),
                    'visible' => ($isAdmin
                                  ? $this->checkbox($item['n_id'], '1')
                                         ->set($item['n_visible'])
                                         ->render()
                                  : ('&nbsp;' . ($item['n_visible']
                                                 ? '&#10003;'
                                                 : '&#10799;'))
                    ),
                    'checkBox' => ($isTrainer
                                   ? $this->checkbox('nabidka[]', $item['n_id'])->render()
                                   : '&nbsp;&#10799;')
                );
            },
            DBNabidka::getNabidka(true)
        );
        $this->render(
            'files/View/Admin/Nabidka/Overview.inc',
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

        $form = $this->checkData($request);
        if (is_object($form)) {
            $this->redirect()->setMessage($form->getMessages());
            $form->displayForm($request);
            return;
        }

        Permissions::checkError('nabidka', P_OWNED, $request->post('trener'));

        $od = $this->date('od')->getPost($request);
        $do = $this->date('do')->getPost($request);
        if (!$do->isValid() || strcmp((string) $od, (string) $do) > 0) {
            $do = $od;
        }

        $visible = (bool) $request->post('visible');
        if (!Permissions::check('nabidka', P_ADMIN) && $visible) {
            $visible = false;
            $this->redirect()->setMessage('Nemáte dostatečná oprávnění ke zviditelnění příspěvku');
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
            $visible,
            $request->post('lock') ? 1 : 0
        );

        $this->redirect(
            $request->post('referer') ?: '/admin/nabidka',
            'Nabídka přidána'
        );
    }
    public function edit($request) {
        $id = $request->getId();
        if (!$id || !($data = DBNabidka::getSingleNabidka($id))) {
            $this->redirect(
                $request->post('referer') ?: '/admin/nabidka',
                'Nabídka s takovým ID neexistuje'
            );
        }
        Permissions::checkError('nabidka', P_OWNED, $data['n_trener']);

        if (!$request->post()) {
            $this->displayForm($request, $data);
            return;
        }

        $form = $this->checkData($request);
        if (is_object($form)) {
            $this->redirect()->setMessage($form->getMessages());
            $form->displayForm($request, $data);
            return;
        }

        $od = $this->date('od')->getPost($request);
        $do = $this->date('do')->getPost($request);
        if (!$do->isValid() || strcmp((string) $od, (string) $do) > 0) {
            $do = $od;
        }

        $visible = (bool) $request->post('visible');
        $visible_prev = $data['n_visible'];
        if (!Permissions::check('nabidka', P_ADMIN)
            && $visible && !$visible_prev
        ) {
            $visible = false;
            $this->redirect()->setMessage('Nemáte dostatečná oprávnění ke zviditelnění nabídky');
        }
        $items = DBNabidka::getNabidkaItemLessons($id);
        $pocet_hod = $request->post('pocet_hod');
        if ($pocet_hod < $items) {
            $pocet_hod = $items;
            $this->redirect()->setMessage(
                'Obsazených hodin už je víc než jste zadali, ' .
                'nemůžu dál snížit počet hodin'
            );
        }
        $max_lessons = $request->post('max_pocet_hod');
        $max_lessons_old = DBNabidka::getNabidkaMaxItems($id);
        if ($max_lessons < $max_lessons_old && $max_lessons != 0) {
            $max_lessons = $max_lessons_old;
            $this->redirect()->setMessage(
                'Zadaný maximální počet hodin/pár je méně než už je zarezervováno, ' .
                'nemůžu dál snížit maximální počet hodin'
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
            $visible,
            $request->post('lock') ? '1' : '0'
        );

        $this->redirect(
            $request->post('referer') ?: '/admin/nabidka',
            'Nabídka úspěšně upravena'
        );
    }

    protected function displayForm($request, $data = null)
    {
        $isAdmin = Permissions::check('nabidka', P_ADMIN);
        if ($isAdmin) {
            $treneri = DBUser::getUsersByPermission('nabidka', P_OWNED);
        } else {
            $treneri = array(DBUser::getUserData(User::getUserID()));
        }
        $this->render(
            'files/View/Admin/Nabidka/Form.inc',
            array(
                'header' => ($request->getAction() == 'add'
                             ? 'Přidat nabídku'
                             : 'Upravit nabídku'),
                'action' => ($request->getAction() == 'add'
                             ? 'Přidat'
                             : 'Upravit'),
                'referer' => $request->getReferer(),
                'users' => $treneri,
                'idAdmin' => $isAdmin ?: '',
                'id' => $data ? $data['n_id'] : null,
                'trener' => ($request->post('trener')
                             ?: $data ? $data['n_trener'] : ''),
                'pocet_hod' => ($request->post('pocet_hod')
                                ?: $data ? $data['n_pocet_hod'] : ''),
                'max_pocet_hod' => ($request->post('max_pocet_hod')
                                    ?: $data ? $data['n_max_pocet_hod'] : ''),
                'od' => ($request->post('od')
                         ?: $data ? $data['n_od'] : ''),
                'do' => ($request->post('do')
                         ?: $data ? $data['n_do'] : ''),
                'visible' => ($request->post('visible')
                              ?: $data ? $data['n_visible'] : false),
                'lock' => ($request->post('lock')
                           ?: $data ? $data['n_lock'] : '')
            )
        );
        return;
    }

    private function checkData($request) {
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

        return $f->isValid() ? true : $f;
    }
}
