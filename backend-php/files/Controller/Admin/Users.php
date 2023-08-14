<?php
namespace Olymp\Controller\Admin;

class Users
{
    public static function edit($id)
    {
        \Permissions::checkError('users', P_ADMIN);
        if (!$data = \DBUser::getUserData($id)) {
            \Message::warning('Uživatel s takovým ID neexistuje');
            \Redirect::to($_POST['returnURI'] ?? '/users');
        }
        if (!$data['u_confirmed']) {
            \Message::warning('Uživatel "' . $data['u_login'] . '" ještě není potvrzený');
            \Redirect::to($_POST['returnURI'] ?? '/users');
        }
        $_POST['login'] = $data['u_login'];
        $_POST['group'] = $data['u_group'];
        $_POST['ban'] = $data['u_ban'];
        $_POST['lock'] = $data['u_lock'];
        $_POST['system'] = $data['u_system'];
        $_POST['dancer'] = $data['u_dancer'];
        $_POST['teacher'] = $data['u_teacher'];
        $_POST['jmeno'] = $data['u_jmeno'];
        $_POST['prijmeni'] = $data['u_prijmeni'];
        $_POST['pohlavi'] = $data['u_pohlavi'];
        $_POST['email'] = $data['u_email'];
        $_POST['telefon'] = $data['u_telefon'];
        $_POST['narozeni'] = $data['u_narozeni'];
        $_POST['rodnecislo'] = $data['u_rodne_cislo'];
        $_POST['skupina'] = $data['u_skupina'];
        $_POST['poznamky'] = $data['u_poznamky'];
        $_POST['street'] = $data['u_street'];
        $_POST['popisne'] = $data['u_conscription_number'];
        $_POST['orientacni'] = $data['u_orientation_number'];
        $_POST['district'] = $data['u_district'];
        $_POST['city'] = $data['u_city'];
        $_POST['postal'] = $data['u_postal_code'];
        $_POST['nationality'] = $data['u_nationality'];
        $_POST['createdAt'] = $data['u_created_at'];
        $_POST['gdprSignedAt'] = $data['u_gdpr_signed_at'];
        return self::displayForm('edit');
    }

    private static function displayForm($action)
    {
        \Render::twig('Admin/UsersForm.twig', [
            'action' => $action,
            'returnURI' => $_POST['returnURI'] ?? ($_SERVER['HTTP_REFERER'] ?? '/users'),
            'countries' => \Countries::$countries,
            'groups' => \Database::queryArray("SELECT * FROM permissions"),
            'skupiny' => \DBSkupiny::get(),
            'login' => $_POST['login'] ?? '',
            'pass' => $_POST['pass'] ?? '',
            'jmeno' => $_POST['jmeno'] ?? '',
            'prijmeni' => $_POST['prijmeni'] ?? '',
            'pohlavi' => $_POST['pohlavi'] ?? '',
            'email' => $_POST['email'] ?? '',
            'telefon' => $_POST['telefon'] ?? '',
            'narozeni' => $_POST['narozeni'] ?? '',
            'rodnecislo' => $_POST['rodnecislo'] ?? '',
            'poznamky' => $_POST['poznamky'] ?? '',
            'street' => $_POST['street'] ?? '',
            'popisne' => $_POST['popisne'] ?? '',
            'orientacni' => $_POST['orientacni'] ?? '',
            'district' => $_POST['district'] ?? '',
            'city' => $_POST['city'] ?? '',
            'postal' => $_POST['postal'] ?? '',
            'nationality' => $_POST['nationality'] ?? '',
            'lock' => $_POST['lock'] ?? '',
            'ban' => $_POST['ban'] ?? '',
            'system' => $_POST['system'] ?? '',
            'group' => $_POST['group'] ?? '',
            'skupina' => $_POST['skupina'] ?? '',
            'dancer' => $_POST['dancer'] ?? '',
            'teacher' => $_POST['teacher'] ?? '',
            'createdAt' => $_POST['createdAt'] ?? '',
            'gdprSignedAt' => $_POST['gdprSignedAt'] ?? ''
        ]);
    }

    private static function checkData($action = 'add'): \Form
    {
        $narozeni = new \Date($_POST['narozeni'] ?? null);

        $f = new \Form();
        $f->checkDate($narozeni, 'Neplatné datum narození');
        $f->checkInArray($_POST['pohlavi'], ['m', 'f'], 'Neplatné pohlaví');
        $f->checkEmail($_POST['email'], 'Neplatný formát emailu');
        $f->checkPhone($_POST['telefon'], 'Neplatný formát telefoního čísla');
        $f->checkNotEmpty($_POST['skupina'], 'Zaškrtněte některou skupinu');

        if ($action == 'add') {
            $f->checkLogin($_POST['login'], 'Špatný formát přihlašovacího jména');
            $f->checkPassword($_POST['pass'], 'Špatný formát hesla');
        }
        return $f;
    }
}
