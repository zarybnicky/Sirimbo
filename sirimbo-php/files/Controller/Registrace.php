<?php
namespace Olymp\Controller;

class Registrace
{
    public static function get()
    {
        $skupiny = array_map(fn($item) => [
            'id' => $item['s_id'],
            'color' => $item['s_color_rgb'],
            'popis' => $item['s_name']
        ], \DBSkupiny::get());

        return new \RenderHelper('files/View/Main/Registrace.inc', [
            'header' => 'Registrace',
            'skupiny' => $skupiny,
            'username' => '',
            'pass' => '',
            'jmeno' => '',
            'prijmeni' => '',
            'pohlavi' => '',
            'email' => '',
            'telefon' => '',
            'narozeni' => '',
            'poznamky' => '',
            'street' => '',
            'popisne' => '',
            'orientacni' => '',
            'district' => '',
            'city' => '',
            'postal' => '',
            'nationality' => '203',
            'dancerName' => '',
            'skupina' => '0',
            'other' => ''
        ]);
    }

    public static function post()
    {
        $narozeni = (string) new \Date($_POST['narozeni'] ?? null);
        $poznamkyMap = [
            'parent' => 'Rodič tanečníka: ' . ($_POST['dancer-name'] ?? null),
            'dancer' => 'Tanečník/tanečnice',
            'other' => 'Jiný vztah: ' . ($_POST['other'] ?? null)
        ];
        $poznamky = $poznamkyMap[$_POST['poznamky'] ?? null] ?? null;

        $f = new \Form();
        $f->checkLogin($_POST['username'], 'Špatný formát přihlašovacího jména', 'username');
        $f->checkPassword($_POST['pass'], 'Špatný formát hesla', 'pass');
        $f->checkEmail($_POST['email'], 'Neplatný formát emailu', 'email');
        $f->checkPhone($_POST['telefon'], 'Neplatný formát telefoního čísla', 'telefon');
        $f->checkDate($narozeni, 'Neplatné datum narození', 'narozeni');
        $f->checkNotEmpty($_POST['orientacni'], 'Vyplňte číslo orientační bydliště', 'orientacni');
        $f->checkNotEmpty($_POST['city'], 'Vyplňte město bydliště', 'city');
        $f->checkNotEmpty($_POST['postal'], 'Vyplňte PSČ bydliště', 'postal');
        $f->checkNotEmpty($_POST['nationality'], 'Vyplňte vaši národnost', 'nationality');

        if (!$f->isValid()) {
            $skupiny = array_map(fn($item) => [
                'id' => $item['s_id'],
                'color' => $item['s_color_rgb'],
                'popis' => $item['s_name']
            ], \DBSkupiny::get());

            new \MessageHelper('warning', $f->getMessages());
            return new \RenderHelper('files/View/Main/Registrace.inc', [
                'header' => 'Registrace',
                'skupiny' => $skupiny,
                'username' => $_POST['username'] ?? '',
                'pass' => $_POST['pass'] ?? '',
                'jmeno' => $_POST['jmeno'] ?? '',
                'prijmeni' => $_POST['prijmeni'] ?? '',
                'pohlavi' => $_POST['pohlavi'] ?? '',
                'email' => $_POST['email'] ?? '',
                'telefon' => $_POST['telefon'] ?? '',
                'narozeni' => $_POST['narozeni'] ?? '',
                'poznamky' => $_POST['poznamky'] ?? '',
                'street' => $_POST['street'] ?? '',
                'popisne' => $_POST['popisne'] ?? '',
                'orientacni' => $_POST['orientacni'] ?? '',
                'district' => $_POST['district'] ?? '',
                'city' => $_POST['city'] ?? '',
                'postal' => $_POST['postal'] ?? '',
                'nationality' => $_POST['nationality'] ?? '',
                'dancerName' => $_POST['dancer-name'] ?? '',
                'skupina' => $_POST['skupina'] ?? '',
                'other' => $_POST['other'] ?? ''
            ]);
        }

        $login = strtolower($_POST['username']);
        \DBUser::addUser(
            $login,
            \User::crypt($_POST['pass']),
            $_POST['jmeno'],
            $_POST['prijmeni'],
            $_POST['pohlavi'],
            $_POST['email'],
            $_POST['telefon'],
            $narozeni,
            $poznamky,
            $_POST['street'],
            $_POST['popisne'],
            $_POST['orientacni'],
            $_POST['district'],
            $_POST['city'],
            $_POST['postal'],
            $_POST['nationality'],
            '0',
            $_POST['skupina'],
            '0',
            '0',
            '0',
            '0',
            ($_POST['poznamky'] === 'dancer') ? '1' : '0',
            '0'
        );

        \Mailer::newUserNotice(DEFAULT_ADMIN_MAIL, $login);
        \Mailer::newUserNotice('hyzam@tkolymp.cz', $login);
        new \MessageHelper(
            'success',
            '<h4 class="alert-heading">Registrace úspěšně proběhla.</h4>' .
            '<p>Během několika dnů vám na email příjde potvrzení vašeho účtu, ' .
            'které vyřizuje administrátor ručně.<p>'
        );
        new \RedirectHelper('/');
    }
}
