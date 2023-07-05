<?php
namespace Olymp\Controller\Member;

class Profil
{
    public static function get()
    {
        \Permissions::checkError('nastenka', P_VIEW);
        $user = \Session::getUser();
        $s = \DBSkupiny::getSingle($user->getTrainingGroup());

        $history = \DBPlatby::getPaymentHistory($user->getId());
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

        \Render::twig('Member/Profil.twig', [
            'user' => $user,
            'ageGroup' => self::getAgeGroup($user->getBirthYear()),
            'coupleData' => \DBPary::getLatestPartner($user->getId(), $user->getGender()),
            'skupina' => [
                'name' => $s['s_name'],
                'color' => $s['s_color_rgb'],
            ],
            'hasPaid' => \DBPlatby::hasPaidMemberFees($user->getId()),
            'paymentHistory' => array_for($history, fn($row) => [
                'id' => $row['pc_id'],
                'name' => $row['pc_name'],
                'symbol' => $row['pc_symbol'],
                'amount' => $row['pi_amount'],
                'paidOn' => $row['pi_date'],
                'validFrom' => $row['pc_valid_from'],
                'validUntil' => $row['pc_valid_to'],
            ]),
            'paymentsWanted' => $paymentsWanted,
        ]);
    }

    public static function getAgeGroup($year): string
    {
        $diff = date('Y') - $year;
        if ($diff < 8) {
            return 'Do 8 let';
        } elseif ($diff < 10) {
            return 'Děti I';
        } elseif (10 <= $diff && $diff < 12) {
            return 'Děti II';
        } elseif (12 <= $diff && $diff < 14) {
            return 'Junioři I';
        } elseif (14 <= $diff && $diff < 16) {
            return 'Junioři II';
        } elseif (16 <= $diff && $diff < 19) {
            return 'Mládež';
        } elseif (19 <= $diff && $diff < 21) {
            return 'Do 21 let';
        } elseif (21 <= $diff && $diff < 35) {
            return 'Dospělí';
        } elseif (35 <= $diff && $diff < 45) {
            return 'Senioři I';
        } elseif (45 <= $diff && $diff < 55) {
            return 'Senioři II';
        } elseif (55 <= $diff && $diff < 65) {
            return 'Senioři III';
        } else {
            return 'Senioři IV';
        }
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
        \DBUser::markGdprSigned(\Session::getUser()->getId());
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
        $user = \Session::getUser();
        $form = self::checkDataEdit();
        if (!$form->isValid()) {
            \Message::warning($form->getMessages());
            return self::renderPersonalForm();
        }

        \DBUser::setUserData(
            \Session::getUser()->getId(),
            $_POST['jmeno'],
            $_POST['prijmeni'],
            $_POST['pohlavi'],
            $_POST['email'],
            $_POST['telefon'],
            (string) new \Date($_POST['narozeni']),
            $_POST['rodnecislo'],
            $user->getNotes(),
            $_POST['street'],
            $_POST['popisne'],
            $_POST['orientacni'],
            $_POST['district'],
            $_POST['city'],
            $_POST['postal'],
            $_POST['nationality'],
            $user->getPermissionGroup(),
            $user->getTrainingGroup(),
            $user->getLocked() ? '1' : '0',
            $user->getBanned() ? '1' : '0',
            $user->getSystem() ? '1' : '0',
            $_POST['dancer'] ? '1' : '0',
            $user->getTeacher() ? '1' : '0',
            $user->getMemberSince(),
            $user->getMemberUntil(),
            $user->getGdprSignedAt()
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
        \DBUser::setPassword(\Session::getUser()->getId(), \User::crypt($_POST['newpass']));
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
        $f = new \Form();
        $f->checkPassword($_POST['newpass'], 'Neplatný formát hesla');
        $f->checkBool(
            \DBUser::checkUser(\Session::getUser()->getLogin(), \User::crypt($_POST['oldpass'])),
            'Staré heslo je špatně',
        );
        $f->checkBool($_POST['newpass'] == $_POST['newpass_confirm'], 'Nová hesla se neshodují');
        return $f;
    }
}
