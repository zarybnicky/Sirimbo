<?php
namespace Olymp\Controller\Member;

class Profil
{
    public static function get()
    {
        \Permissions::checkError('nastenka', P_VIEW);
        $user = \Session::getUser();
        $s = \DBSkupiny::getSingle($user->getTrainingGroup());

        $history = \Database::queryArray(
            "SELECT * FROM platby_item INNER JOIN platby_category ON pi_id_category=pc_id
             WHERE pi_id_user='?'
             ORDER BY pi_date DESC",
            $user->getId(),
        );
        $paymentsPaid = array_flip(array_column($history, 'pc_id'));
        $paymentsWanted = [];
        $groups = \DBSkupiny::getSingleWithCategories($user->getTrainingGroup());
        foreach ($groups as $row) {
            if (!$row['pc_visible'] || isset($paymentsPaid[$row['pc_id']])) {
                continue;
            }
            $paymentsWanted[] = [
                'name' => $row['pc_name'],
                'type' => $row['pg_type'] ? 'Členské příspěvky' : 'Ostatní platby',
                'symbol' => $row['pc_symbol'],
                'amount' => $row['pc_amount'] * ($row['pc_use_base'] ? $row['pg_base'] : 1),
                'dueDate' => $row['pc_date_due'],
            ];
        }

        $row = \Database::querySingle(
            "SELECT COUNT(*) as count FROM platby_item
                INNER JOIN platby_category ON pc_id=pi_id_category
                INNER JOIN platby_category_group ON pcg_id_category=pc_id
                INNER JOIN platby_group ON pg_id=pcg_id_group
                INNER JOIN platby_group_skupina ON pgs_id_group=pg_id
                INNER JOIN skupiny ON pgs_id_skupina=s_id
                INNER JOIN users ON pi_id_user=u_id
            WHERE
                pg_type='1' AND
                u_id='?' AND
                u_skupina=s_id AND
                CURRENT_DATE >= pc_valid_from AND
                CURRENT_DATE <= pc_valid_to",
            $user->getId(),
        );
        $hasPaid = !!$row['count'];

        \Render::twig('Member/Profil.twig', [
            'user' => $user,
            'skupina' => [
                'name' => $s['s_name'],
                'color' => $s['s_color_rgb'],
            ],
            'hasPaid' => $hasPaid,
            'paymentHistory' => array_map(
                fn($row) => [
                    'id' => $row['pc_id'],
                    'name' => $row['pc_name'],
                    'symbol' => $row['pc_symbol'],
                    'amount' => $row['pi_amount'],
                    'paidOn' => $row['pi_date'],
                    'validFrom' => $row['pc_valid_from'],
                    'validUntil' => $row['pc_valid_to'],
                ],
                $history,
            ),
            'paymentsWanted' => $paymentsWanted,
        ]);
    }

    public static function renderPersonalForm()
    {
        \Render::twig('Member/ProfilPersonalData.twig', [
            'countries' => \Countries::$countries,
            'lock' => $_POST['lock'] ?? false,
            'jmeno' => $_POST['jmeno'] ?? '',
            'prijmeni' => $_POST['prijmeni'] ?? '',
            'pohlavi' => $_POST['pohlavi'] ?? null,
            'email' => $_POST['email'] ?? '',
            'telefon' => $_POST['telefon'] ?? '',
            'narozeni' => $_POST['narozeni'] ?? '',
            'rodnecislo' => $_POST['rodnecislo'] ?? '',
            'street' => $_POST['street'] ?? '',
            'popisne' => $_POST['popisne'] ?? '',
            'orientacni' => $_POST['orientacni'] ?? '',
            'city' => $_POST['city'] ?? '',
            'district' => $_POST['district'] ?? '',
            'postal' => $_POST['postal'] ?? '',
            'nationality' => $_POST['nationality'] ?? '',
            'dancer' => $_POST['dancer'] ?? '',
            'returnURI' => $_SERVER['HTTP_REFERER'] ?? '/member',
        ]);
    }

    public static function gdpr()
    {
        \Permissions::checkError('nastenka', P_VIEW);
        \Render::twig('Member/ProfilGdpr.twig');
    }

    public static function gdprPost()
    {
        \Permissions::checkError('nastenka', P_VIEW);
        \Database::query("UPDATE users SET u_gdpr_signed_at=NOW() WHERE u_id='?'", \Session::getUser()->getId());
        \Redirect::to('/member');
    }

    public static function edit()
    {
        \Permissions::checkError('nastenka', P_VIEW);
        $user = \Session::getUser();
        $_POST['jmeno'] = $user->getName();
        $_POST['prijmeni'] = $user->getSurname();
        $_POST['pohlavi'] = $user->getGender();
        $_POST['narozeni'] = $user->getBirthDate();
        $_POST['rodnecislo'] = $user->getBirthNumber();
        $_POST['email'] = $user->getEmail();
        $_POST['telefon'] = $user->getPhone();
        $_POST['street'] = $user->getStreet();
        $_POST['popisne'] = $user->getConscriptionNumber();
        $_POST['orientacni'] = $user->getOrientationNumber();
        $_POST['city'] = $user->getCity();
        $_POST['district'] = $user->getDistrict();
        $_POST['postal'] = $user->getPostalCode();
        $_POST['nationality'] = $user->getNationality();
        $_POST['dancer'] = $user->getDancer();
        return self::renderPersonalForm();
    }

    public static function editPost()
    {
        \Permissions::checkError('nastenka', P_VIEW);
        $form = self::checkDataEdit();
        if (!$form->isValid()) {
            \Message::warning($form->getMessages());
            return self::renderPersonalForm();
        }

        \Database::query(
            "UPDATE users SET u_jmeno='?',u_prijmeni='?',u_pohlavi='?',u_email='?'," .
            "u_telefon='?',u_narozeni='?',u_rodne_cislo='?', u_street='?'," .
            "u_conscription_number='?',u_orientation_number='?',u_district='?',u_city='?',u_postal_code='?'," .
            "u_nationality='?',u_dancer='?'" .
            " WHERE u_id='?'",
            $_POST['jmeno'],
            $_POST['prijmeni'],
            $_POST['pohlavi'],
            $_POST['email'],
            $_POST['telefon'],
            (string) new \Date($_POST['narozeni']),
            $_POST['rodnecislo'],
            $_POST['street'],
            $_POST['popisne'],
            $_POST['orientacni'],
            $_POST['district'],
            $_POST['city'],
            $_POST['postal'],
            $_POST['nationality'],
            $_POST['dancer'] ? '1' : '0',
            \Session::getUser()->getId(),
        );
        \Redirect::to('/member/profil');
    }

    public static function heslo()
    {
        \Permissions::checkError('nastenka', P_VIEW);
        \Render::twig('Member/ProfilNewPassword.twig');
    }

    public static function hesloPost()
    {
        \Permissions::checkError('nastenka', P_VIEW);
        $form = self::checkDataHeslo();
        if (!$form->isValid()) {
            \Message::warning($form->getMessages());
            \Render::twig('Member/ProfilNewPassword.twig');
            return;
        }
        \Database::query("select reset_password('?', '?')", \Session::getUser()->getId(), \User::crypt($_POST['newpass']));
        \Redirect::to('/member/profil');
    }

    private static function checkDataEdit(): \Form
    {
        $f = new \Form();
        $f->checkDate((string) new \Date($_POST['narozeni']), 'Neplatné datum narození');
        $f->checkNotEmpty($_POST['rodnecislo'], 'Neplatné rodné číslo');
        $f->checkInArray($_POST['pohlavi'], ['m', 'f'], 'Neplatné pohlaví');
        $f->checkEmail($_POST['email'], 'Neplatný formát emailu');
        $f->checkPhone($_POST['telefon'], 'Neplatný formát telefoního čísla');
        $f->checkNumeric($_POST['nationality'], 'Neplatný formát národnosti');
        $f->checkNotEmpty($_POST['city'], 'Zadejte město bydliště');
        $f->checkNumeric(str_replace(' ', '', $_POST['postal']), 'Zadejte číselné PSČ');
        return $f;
    }

    private static function checkDataHeslo(): \Form
    {
        $data = \Database::querySingle("SELECT * FROM users WHERE LOWER(u_login)='?' AND u_pass='?'", \Session::getUser()->getLogin(), \User::crypt($_POST['oldpass']));
        $f = new \Form();
        $f->checkPassword($_POST['newpass'], 'Neplatný formát hesla');
        $f->checkBool($data, 'Staré heslo je špatně');
        $f->checkBool($_POST['newpass'] == $_POST['newpass_confirm'], 'Nová hesla se neshodují');
        return $f;
    }
}
