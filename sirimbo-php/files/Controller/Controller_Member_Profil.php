<?php
class Controller_Member_Profil
{
    public function view($request)
    {
        \Permissions::checkError('nastenka', P_VIEW);
        $data = \Session::getUserData();
        $s = \Session::getSkupinaData();

        $paymentsPaid = [];
        $paymentHistory = array_map(
            function ($row) use (&$paymentsPaid) {
                $paymentsPaid[$row['pc_id']] = '';
                return [
                    'type' => $row['pc_name'],
                    'varSymbol' => $row['pc_symbol'],
                    'amount' => $row['pi_amount'],
                    'paidOn' => formatDate($row['pi_date']),
                    'validFor' => formatDate($row['pc_valid_from']) . ' - ' . formatDate($row['pc_valid_to']),
                ];
            },
            \DBPlatby::getPaymentHistory(\Session::getUserID())
        );

        $paymentsWanted = [];
        $groups = \DBSkupiny::getSingleWithCategories(\Session::getSkupina());
        foreach ($groups as $row) {
            if (!$row['pc_visible'] || isset($paymentsPaid[$row['pc_id']])) {
                continue;
            }
            $paymentsWanted[] = [
                'name' => $row['pc_name'],
                'type' => $row['pg_type'] ? 'Členské příspěvky' : 'Ostatní platby',
                'symbol' => $row['pc_symbol'],
                'amount' => ($row['pc_use_base'] ? ($row['pc_amount'] * $row['pg_base']) : $row['pc_amount']) . ' Kč',
                'dueDate' => (new \Date($row['pc_date_due']))->getHumanDate(),
                'validRange' => ((new \Date($row['pc_valid_from']))->getHumanDate() .
                    ((new \Date($row['pc_valid_to']))->isValid() ?
                        (' - ' . (new \Date($row['pc_valid_to']))->getHumanDate()) : ''))
            ];
        }

        new \RenderHelper('files/View/Member/Profil/Overview.inc', [
            'header' => $data->getFullName(),
            'ageGroup' => \Session::getAgeGroup($data->getBirthYear()),
            'coupleData' => \Session::getCoupleData(),
            'skupina' => new \ColorboxHelper($s['s_color_rgb'], $s['s_name']) . '&nbsp;' . $s['s_name'],
            'varSymbol' => \User::varSymbol(\Session::getUserID()),
            'hasPaid' => \Session::getZaplaceno(),
            'paymentHistory' => $paymentHistory,
            'paymentsWanted' => $paymentsWanted,
        ]);
    }

    public static function renderPersonalForm($request)
    {
        new \RenderHelper('files/View/Member/Profil/PersonalData.inc', [
            'header' => 'Osobní údaje',
            'lock' => $_POST['lock'],
            'jmeno' => $_POST['jmeno'],
            'prijmeni' => $_POST['prijmeni'],
            'pohlavi' => $_POST['pohlavi'],
            'email' => $_POST['email'],
            'telefon' => $_POST['telefon'],
            'narozeni' => $_POST['narozeni'],
            'street' => $_POST['street'],
            'popisne' => $_POST['popisne'],
            'orientacni' => $_POST['orientacni'],
            'city' => $_POST['city'],
            'district' => $_POST['district'],
            'postal' => $_POST['postal'],
            'nationality' => $_POST['nationality'],
            'dancer' => $_POST['dancer'],
            'returnURI' => $request->getReferer() ?: '/member',
        ]);
    }

    public function gdpr($request)
    {
        \Permissions::checkError('nastenka', P_VIEW);
        if ($_POST['action'] !== 'gdpr') {
            return new \RenderHelper('files/View/Member/Profil/Gdpr.inc', [
                'header' => 'Souhlas se zpracováním osobních údajů',
            ]);
        }
        \DBUser::markGdprSigned(\Session::getUserId());
        new \RedirectHelper('/member');
    }

    public function edit($request)
    {
        \Permissions::checkError('nastenka', P_VIEW);
        $data = \Session::getUserData();
        $narozeni = new \Date($_POST['narozeni']);

        if (!$_POST) {
            $_POST['jmeno'] = $data->getName();
            $_POST['prijmeni'] = $data->getSurname();
            $_POST['pohlavi'] = $data->getGender();
            $_POST['narozeni'] = $data->getBirthDate();
            $_POST['email'] = $data->getEmail();
            $_POST['telefon'] = $data->getPhone();
            $_POST['street'] = $data->getStreet();
            $_POST['popisne'] = $data->getConscriptionNumber();
            $_POST['orientacni'] = $data->getOrientationNumber();
            $_POST['city'] = $data->getCity();
            $_POST['district'] = $data->getDistrict();
            $_POST['postal'] = $data->getPostalCode();
            $_POST['nationality'] = $data->getNationality();
            $_POST['dancer'] = $data->getDancer();
            return static::renderPersonalForm($request);
        }

        $form = static::checkData($request, 'edit', $narozeni);
        if (!$form->isValid()) {
            new \MessageHelper('warning', $form->getMessages());
            return static::renderPersonalForm($request);
        }

        \DBUser::setUserData(
            \Session::getUserID(),
            $_POST['jmeno'],
            $_POST['prijmeni'],
            $_POST['pohlavi'],
            $_POST['email'],
            $_POST['telefon'],
            (string) $narozeni,
            $data->getNotes(),
            $_POST['street'],
            $_POST['popisne'],
            $_POST['orientacni'],
            $_POST['district'],
            $_POST['city'],
            $_POST['postal'],
            $_POST['nationality'],
            $data->getPermissionGroup(),
            $data->getTrainingGroup(),
            $data->getLocked() ? '1' : '0',
            $data->getBanned() ? '1' : '0',
            $data->getSystem() ? '1' : '0',
            $_POST['dancer'] ? '1' : '0',
            $data->getTeacher() ? '1' : '0',
            $data->getMemberSince(),
            $data->getMemberUntil(),
            $data->getGdprSignedAt()
        );
        new \RedirectHelper('/member/profil');
    }

    public function heslo($request)
    {
        \Permissions::checkError('nastenka', P_VIEW);
        if (!$_POST) {
            return new \RenderHelper('files/View/Member/Profil/NewPassword.inc', [
                'header' => 'Změna hesla'
            ]);
        }
        $form = static::checkData($request, 'heslo');
        if (!$form->isValid()) {
            new \MessageHelper('warning', $form->getMessages());
            return new \RenderHelper('files/View/Member/Profil/NewPassword.inc', [
                'header' => 'Změna hesla'
            ]);
        }
        \DBUser::setPassword(
            \Session::getUserID(),
            \User::crypt($_POST['newpass'])
        );
        new \RedirectHelper('/member/profil');
    }

    private static function checkData($request, $action, $narozeni = null): \Form
    {
        $f = new \Form();
        if ($action == 'edit') {
            $f->checkDate((string) $narozeni, 'Neplatné datum narození', 'narozeni');
            $f->checkInArray($_POST['pohlavi'], ['m', 'f'], 'Neplatné pohlaví', 'pohlavi');
            $f->checkEmail($_POST['email'], 'Neplatný formát emailu', 'email');
            $f->checkPhone($_POST['telefon'], 'Neplatný formát telefoního čísla', 'telefon');
            $f->checkNumeric($_POST['nationality'], 'Neplatný formát národnosti', 'nationality');
            $f->checkNotEmpty($_POST['city'], 'Zadejte město bydliště', 'city');
            $f->checkNumeric(
                str_replace(' ', '', $_POST['postal']),
                'Zadejte číselné PSČ',
                'postal'
            );
        } elseif ($action == 'heslo') {
            $f->checkPassword($_POST['newpass'], 'Neplatný formát hesla', 'newpass');
            $f->checkBool(
                \DBUser::checkUser(
                    \Session::getUserData()->getLogin(),
                    \User::crypt($_POST['oldpass'])
                ),
                'Staré heslo je špatně',
                'oldpass'
            );
            $f->checkBool(
                $_POST['newpass'] == $_POST['newpass_confirm'],
                'Nová hesla se neshodují',
                'newpass_check'
            );
        }
        return $f;
    }
}
