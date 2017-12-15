<?php
require_once 'files/Controller/Member.php';
class Controller_Member_Profil extends Controller_Member
{
    public function __construct()
    {
        Permissions::checkError('nastenka', P_VIEW);
    }

    public function view($request)
    {
        $this->render('files/View/Member/Profil/Overview.inc');
    }

    public function edit($request)
    {
        $data = DBUser::getUserData(User::getUserID());
        $narozeni = $this->date('narozeni')->getPost($request);

        if (!$request->post()) {
            $this->render(
                'files/View/Member/Profil/PersonalData.inc',
                [
                    'login' => User::getUserName(),
                    'group' => $data['u_group'],
                    'lock' => $data['u_lock'],
                    'jmeno' => $data['u_jmeno'],
                    'prijmeni' => $data['u_prijmeni'],
                    'pohlavi' => $data['u_pohlavi'],
                    'email' => $data['u_email'],
                    'telefon' => $data['u_telefon'],
                    'narozeni' => $data['u_narozeni'],
                    'poznamky' => $data['u_poznamky']
                ]
            );
            return;
        }

        if (is_object($f = $this->checkData($request, 'edit', $narozeni))) {
            $this->redirect()->setMessage($f->getMessages());
            $this->render(
                'files/View/Member/Profil/PersonalData.inc',
                [
                    'login' => $request->post('login'),
                    'group' => $request->post('group'),
                    'lock' => $request->post('lock'),
                    'jmeno' => $request->post('jmeno'),
                    'prijmeni' => $request->post('prijmeni'),
                    'email' => $request->post('email'),
                    'telefon' => $request->post('telefon'),
                    'narozeni' => $request->post('narozeni'),
                    'poznamky' => $request->post('poznamky')
                ]
            );
            return;
        }

        DBUser::setUserData(
            User::getUserID(),
            $request->post('jmeno'),
            $request->post('prijmeni'),
            $request->post('pohlavi'),
            $request->post('email'),
            $request->post('telefon'),
            (string) $narozeni,
            $request->post('poznamky'),
            $data['u_group'],
            $data['u_skupina'],
            $data['u_dancer'],
            $data['u_lock'],
            $data['u_ban'],
            $data['u_system']
        );
        $this->redirect('/member/profil', 'Upraveno');
    }

    public function heslo($request)
    {
        if (!$request->post() ||
            is_object($f = $this->checkData($request, 'heslo'))
        ) {
            if ($request->post()) {
                $this->redirect()->setMessage($f->getMessages());
            }
            $this->render('files/View/Member/Profil/NewPassword.inc');
            return;
        }
        DBUser::setPassword(
            User::getUserID(),
            User::crypt($request->post('newpass'))
        );
        $this->redirect('/member/profil', 'Heslo změněno');
    }

    public function platby($request)
    {
        $groupsOut = [];
        $groups = DBSkupiny::getSingleWithCategories(User::getSkupina());
        $currentGroup = 0;
        foreach ($groups as $row) {
            if (!$row['pc_visible']) {
                continue;
            }
            if ($currentGroup != $row['pg_id']) {
                $groupsOut[] = [
                    'name' => new Tag(
                        'span',
                        ['class' => 'big', 'style' => 'text-decoration:underline'],
                        $row['pg_name']
                    ],
                    'type' => (!$row['pg_type']
                               ? 'Ostatní platby'
                               : 'Členské příspěvky'),
                    'symbol' => '',
                    'amount' => '',
                    'dueDate' => '',
                    'validRange' => ''
                );
                $currentGroup = $row['pg_id'];
            }
            $groupsOut[] = [
                'name' => $row['pc_name'],
                'type' => '',
                'symbol' => $row['pc_symbol'],
                'amount' => (($row['pc_use_base'] ? ($row['pc_amount'] * $row['pg_base']) : $row['pc_amount']) . ' Kč'),
                'dueDate' => (new Date($row['pc_date_due']))->getDate(Date::FORMAT_SIMPLIFIED),
                'validRange' => ((new Date($row['pc_valid_from']))->getDate(Date::FORMAT_SIMPLIFIED) .
                    ((new Date($row['pc_valid_to']))->isValid() ?
                        (' - ' . (new Date($row['pc_valid_to']))->getDate(Date::FORMAT_SIMPLIFIED)) : ''))
            ];
        }
        $skupina = User::getSkupinaData();
        $this->render(
            'files/View/Member/Profil/Platby.inc',
            [
                'colorBox' => $this->colorbox($skupina['s_color_rgb'], $skupina['s_description']),
                'skupinaData' => $skupina['s_name'],
                'varSymbol' => User::varSymbol(User::getUserID()),
                'zaplacenoText' => User::getZaplaceno() ? 'zaplaceno' : 'nezaplaceno!',
                'platby' => [],
                'platbyGroups' => $groupsOut
            ]
        );
    }

    private function checkData($request, $action, $narozeni = null)
    {
        $f = new Form();
        if ($action == 'edit') {
            $f->checkDate(
                (string) $narozeni,
                'Neplatné datum narození',
                'narozeni'
            );
            $f->checkLength(
                $request->post('jmeno'),
                1,
                40,
                'Špatná délka jména',
                'jmeno'
            );
            $f->checkLength(
                $request->post('prijmeni'),
                1,
                40,
                'Špatná délka přijmení',
                'prijmeni'
            );
            $f->checkInArray(
                $request->post('pohlavi'),
                ['m', 'f'],
                'Neplatné pohlaví',
                'pohlavi'
            );
            $f->checkEmail(
                $request->post('email'),
                'Neplatný formát emailu',
                'email'
            );
            $f->checkPhone(
                $request->post('telefon'),
                'Neplatný formát telefoního čísla',
                'telefon'
            );
        } elseif ($action == 'heslo') {
            $f->checkPassword(
                $request->post('newpass'),
                'Neplatný formát hesla',
                'newpass'
            );
            $f->checkBool(
                DBUser::checkUser(
                    User::getUserName(),
                    User::crypt($request->post('oldpass'))
                ),
                'Staré heslo je špatně',
                'oldpass'
            );
            $f->checkBool(
                $request->post('newpass') == $request->post('newpass_confirm'),
                'Nová hesla se neshodují',
                'newpass_check'
            );
        }
        return $f->isValid() ? null : $f;
    }
}
