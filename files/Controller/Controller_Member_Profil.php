<?php
class Controller_Member_Profil extends Controller_Abstract
{
    public function __construct()
    {
        Permissions::checkError('nastenka', P_VIEW);
    }

    public function view($request)
    {
        $data = User::getUserData();
        $s = User::getSkupinaData();

        $groupsOut = [];
        $groups = DBSkupiny::getSingleWithCategories(User::getSkupina());
        foreach ($groups as $row) {
            if (!$row['pc_visible']) {
                continue;
            }
            $groupsOut[] = [
                'name' => $row['pc_name'],
                'type' => $row['pg_type'] ? 'Členské příspěvky' : 'Ostatní platby',
                'symbol' => $row['pc_symbol'],
                'amount' => ($row['pc_use_base'] ? ($row['pc_amount'] * $row['pg_base']) : $row['pc_amount']) . ' Kč',
                'dueDate' => (new Date($row['pc_date_due']))->getDate(Date::FORMAT_SIMPLIFIED),
                'validRange' => ((new Date($row['pc_valid_from']))->getDate(Date::FORMAT_SIMPLIFIED) .
                    ((new Date($row['pc_valid_to']))->isValid() ?
                        (' - ' . (new Date($row['pc_valid_to']))->getDate(Date::FORMAT_SIMPLIFIED)) : ''))
            ];
        }

        $platby = array_map(
            function ($row) {
                return [
                    'type' => $row['pc_name'],
                    'varSymbol' => $row['pc_symbol'],
                    'amount' => $row['pi_amount'],
                    'paidOn' => formatDate($row['pi_date']),
                    'validFor' => formatDate($row['pc_valid_from']) . ' - ' . formatDate($row['pc_valid_to']),
                ];
            },
            DBPlatby::getPaymentHistory(User::getUserID())
        );

        $this->render('files/View/Member/Profil/Overview.inc', [
            'header' => $data['u_jmeno'] . ' ' . $data['u_prijmeni'],
            'ageGroup' => User::getAgeGroup(explode('-', $data['u_narozeni'])[0]),
            'coupleData' => User::getCoupleData(),
            'skupina' => (
                $this->colorbox($s['s_color_rgb'], $s['s_name']) .
                '&nbsp;' . $s['s_name']
            ),
            'varSymbol' => User::varSymbol(User::getUserID()),
            'hasPaid' => User::getZaplaceno(),
            'platby' => $platby,
            'platbyGroups' => $groupsOut,
        ]);
    }

    public function renderPersonalForm($request)
    {
        $this->render('files/View/Member/Profil/PersonalData.inc', [
            'header' => 'Osobní údaje',
            'lock' => $request->post('lock'),
            'jmeno' => $request->post('jmeno'),
            'prijmeni' => $request->post('prijmeni'),
            'pohlavi' => $request->post('pohlavi'),
            'email' => $request->post('email'),
            'telefon' => $request->post('telefon'),
            'narozeni' => $request->post('narozeni'),
            'street' => $request->post('street'),
            'popisne' => $request->post('popisne'),
            'orientacni' => $request->post('orientacni'),
            'city' => $request->post('city'),
            'district' => $request->post('district'),
            'postal' => $request->post('postal'),
            'nationality' => $request->post('nationality'),
            'dancer' => $request->post('dancer'),
        ]);
    }

    public function edit($request)
    {
        $data = DBUser::getUserData(User::getUserID());
        $narozeni = $this->date('narozeni')->getPost($request);

        if (!$request->post()) {
            $request->post('jmeno', $data['u_jmeno']);
            $request->post('prijmeni', $data['u_prijmeni']);
            $request->post('pohlavi', $data['u_pohlavi']);
            $request->post('narozeni', $data['u_narozeni']);
            $request->post('email', $data['u_email']);
            $request->post('telefon', $data['u_telefon']);
            $request->post('street', $data['u_street']);
            $request->post('popisne', $data['u_conscription_number']);
            $request->post('orientacni', $data['u_orientation_number']);
            $request->post('city', $data['u_city']);
            $request->post('district', $data['u_district']);
            $request->post('postal', $data['u_postal_code']);
            $request->post('nationality', $data['u_nationality']);
            $request->post('dancer', $data['u_dancer']);
            return $this->renderPersonalForm($request);
        }

        $form = $this->checkData($request, 'edit', $narozeni);
        if (!$form->isValid()) {
            $this->redirect()->warning($form->getMessages());
            return $this->renderPersonalForm($request);
        }

        DBUser::setUserData(
            User::getUserID(),
            $request->post('jmeno'),
            $request->post('prijmeni'),
            $request->post('pohlavi'),
            $request->post('email'),
            $request->post('telefon'),
            (string) $narozeni,
            $data['u_poznamky'],
            $request->post('street'),
            $request->post('popisne'),
            $request->post('orientacni'),
            $request->post('district'),
            $request->post('city'),
            $request->post('postal'),
            $request->post('nationality'),
            $data['u_group'],
            $data['u_skupina'],
            $data['u_lock'],
            $data['u_ban'],
            $data['u_system'],
            $request->post('dancer') ? '1' : '0',
            $data['u_teacher'],
            $data['u_member_since'],
            $data['u_member_until'],
            $data['u_gdpr_signed_at']
        );
        $this->redirect('/member/profil');
    }

    public function heslo($request)
    {
        if (!$request->post()) {
            return $this->render('files/View/Member/Profil/NewPassword.inc', [
                'header' => 'Změna hesla'
            ]);
        }
        $form = $this->checkData($request, 'heslo');
        if (!$form->isValid()) {
            $this->redirect()->warning($form->getMessages());
            return $this->render('files/View/Member/Profil/NewPassword.inc', [
                'header' => 'Změna hesla'
            ]);
        }
        DBUser::setPassword(
            User::getUserID(),
            User::crypt($request->post('newpass'))
        );
        $this->redirect('/member/profil');
    }

    private function checkData($request, $action, $narozeni = null): Form
    {
        $f = new Form();
        if ($action == 'edit') {
            $f->checkDate((string) $narozeni, 'Neplatné datum narození', 'narozeni');
            $f->checkInArray($request->post('pohlavi'), ['m', 'f'], 'Neplatné pohlaví', 'pohlavi');
            $f->checkEmail($request->post('email'), 'Neplatný formát emailu', 'email');
            $f->checkPhone($request->post('telefon'), 'Neplatný formát telefoního čísla', 'telefon');
        } elseif ($action == 'heslo') {
            $f->checkPassword($request->post('newpass'), 'Neplatný formát hesla', 'newpass');
            $f->checkBool(
                DBUser::checkUser(
                    User::getUserData()['u_login'],
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
        return $f;
    }
}
