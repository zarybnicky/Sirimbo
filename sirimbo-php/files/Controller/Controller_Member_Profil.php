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

    public function renderPersonalForm($request)
    {
        new \RenderHelper('files/View/Member/Profil/PersonalData.inc', [
            'header' => 'Osobní údaje',
            'lock' => $_POST['lock'] ?? null,
            'jmeno' => $_POST['jmeno'] ?? null,
            'prijmeni' => $_POST['prijmeni'] ?? null,
            'pohlavi' => $_POST['pohlavi'] ?? null,
            'email' => $_POST['email'] ?? null,
            'telefon' => $_POST['telefon'] ?? null,
            'narozeni' => $_POST['narozeni'] ?? null,
            'street' => $_POST['street'] ?? null,
            'popisne' => $_POST['popisne'] ?? null,
            'orientacni' => $_POST['orientacni'] ?? null,
            'city' => $_POST['city'] ?? null,
            'district' => $_POST['district'] ?? null,
            'postal' => $_POST['postal'] ?? null,
            'nationality' => $_POST['nationality'] ?? null,
            'dancer' => $_POST['dancer'] ?? null,
            'returnURI' => $request->getReferer() ?: '/member',
        ]);
    }

    public function gdpr($request)
    {
        \Permissions::checkError('nastenka', P_VIEW);
        if ($_POST['action'] ?? null !== 'gdpr') {
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
        $narozeni = new \Date($_POST['narozeni'] ?? null);

        if (!$request->post()) {
            $request->post('jmeno', $data->getName());
            $request->post('prijmeni', $data->getSurname());
            $request->post('pohlavi', $data->getGender());
            $request->post('narozeni', $data->getBirthDate());
            $request->post('email', $data->getEmail());
            $request->post('telefon', $data->getPhone());
            $request->post('street', $data->getStreet());
            $request->post('popisne', $data->getConscriptionNumber());
            $request->post('orientacni', $data->getOrientationNumber());
            $request->post('city', $data->getCity());
            $request->post('district', $data->getDistrict());
            $request->post('postal', $data->getPostalCode());
            $request->post('nationality', $data->getNationality());
            $request->post('dancer', $data->getDancer());
            return $this->renderPersonalForm($request);
        }

        $form = $this->checkData($request, 'edit', $narozeni);
        if (!$form->isValid()) {
            new \MessageHelper('warning', $form->getMessages());
            return $this->renderPersonalForm($request);
        }

        \DBUser::setUserData(
            \Session::getUserID(),
            $_POST['jmeno'] ?? null,
            $_POST['prijmeni'] ?? null,
            $_POST['pohlavi'] ?? null,
            $_POST['email'] ?? null,
            $_POST['telefon'] ?? null,
            (string) $narozeni,
            $data->getNotes(),
            $_POST['street'] ?? null,
            $_POST['popisne'] ?? null,
            $_POST['orientacni'] ?? null,
            $_POST['district'] ?? null,
            $_POST['city'] ?? null,
            $_POST['postal'] ?? null,
            $_POST['nationality'] ?? null,
            $data->getPermissionGroup(),
            $data->getTrainingGroup(),
            $data->getLocked() ? '1' : '0',
            $data->getBanned() ? '1' : '0',
            $data->getSystem() ? '1' : '0',
            $_POST['dancer'] ?? null ? '1' : '0',
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
        if (!$request->post()) {
            return new \RenderHelper('files/View/Member/Profil/NewPassword.inc', [
                'header' => 'Změna hesla'
            ]);
        }
        $form = $this->checkData($request, 'heslo');
        if (!$form->isValid()) {
            new \MessageHelper('warning', $form->getMessages());
            return new \RenderHelper('files/View/Member/Profil/NewPassword.inc', [
                'header' => 'Změna hesla'
            ]);
        }
        \DBUser::setPassword(
            \Session::getUserID(),
            \User::crypt($_POST['newpass'] ?? null)
        );
        new \RedirectHelper('/member/profil');
    }

    private function checkData($request, $action, $narozeni = null): \Form
    {
        $f = new \Form();
        if ($action == 'edit') {
            $f->checkDate((string) $narozeni, 'Neplatné datum narození', 'narozeni');
            $f->checkInArray($_POST['pohlavi'] ?? null, ['m', 'f'], 'Neplatné pohlaví', 'pohlavi');
            $f->checkEmail($_POST['email'] ?? null, 'Neplatný formát emailu', 'email');
            $f->checkPhone($_POST['telefon'] ?? null, 'Neplatný formát telefoního čísla', 'telefon');
            $f->checkNumeric($_POST['nationality'] ?? null, 'Neplatný formát národnosti', 'nationality');
            $f->checkNotEmpty($_POST['city'] ?? null, 'Zadejte město bydliště', 'city');
            $f->checkNumeric(
                str_replace(' ', '', $_POST['postal'] ?? null),
                'Zadejte číselné PSČ',
                'postal'
            );
        } elseif ($action == 'heslo') {
            $f->checkPassword($_POST['newpass'] ?? null, 'Neplatný formát hesla', 'newpass');
            $f->checkBool(
                \DBUser::checkUser(
                    \Session::getUserData()->getLogin(),
                    \User::crypt($_POST['oldpass'] ?? null)
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
