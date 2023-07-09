<?php
namespace Olymp\Controller\Admin;

class Users
{
    public static function list()
    {
        \Permissions::checkError('users', P_ADMIN);
        \Render::twig('Admin/Users.twig');
    }

    public static function remove($id)
    {
        \Permissions::checkError('users', P_ADMIN);
        $item = \DBUser::getUserData($id);
        \Render::twig('RemovePrompt.twig', [
            'header' => 'Správa uživatelů',
            'prompt' => 'Opravdu chcete odstranit uživatele:',
            'returnURI' => $_SERVER['HTTP_REFERER'] ?? '/admin/users',
            'data' => [[
                'id' => $item['u_id'],
                'text' => "{$item['u_jmeno']} {$item['u_prijmeni']} ({$item['u_login']})",
            ]]
        ]);
    }

    public static function removePost($id)
    {
        \Permissions::checkError('users', P_ADMIN);
        \Database::query("DELETE FROM users WHERE u_id='?'", $id);
        \Database::query("DELETE FROM rozpis WHERE r_trener='?'", $id);
        \Database::query("DELETE FROM rozpis_item WHERE ri_partner='?'", $id);
        \Database::query("DELETE FROM nabidka WHERE n_trener='?'", $id);
        \Database::query("DELETE FROM nabidka_item WHERE ni_partner='?'", $id);
        \Database::query("DELETE FROM attendee_user WHERE user_id='?'", $id);
        \DBPary::noPartner($id);
        \Database::query("DELETE FROM pary WHERE p_id_partner='?' AND p_archiv='0'", $id);
        \Redirect::to($_POST['returnURI'] ?? '/admin/users');
    }

    public static function add()
    {
        \Permissions::checkError('users', P_ADMIN);
        return self::displayForm('add');
    }

    public static function addPost()
    {
        \Permissions::checkError('users', P_ADMIN);
        $form = self::checkData('add');
        if (!$form->isValid()) {
            \Message::warning($form->getMessages());
            return self::displayForm('add');
        }
        \DBUser::addUser(
            strtolower($_POST['login']),
            \User::crypt($_POST['pass']),
            $_POST['jmeno'],
            $_POST['prijmeni'],
            $_POST['pohlavi'],
            $_POST['email'],
            $_POST['telefon'],
            (string) new \Date($_POST['narozeni'] ?? null),
            $_POST['rodnecislo'],
            $_POST['poznamky'],
            $_POST['street'],
            $_POST['popisne'],
            $_POST['orientacni'],
            $_POST['district'],
            $_POST['city'],
            $_POST['postal'],
            $_POST['nationality'],
            $_POST['group'],
            $_POST['skupina'],
            $_POST['lock'] ? '1' : '0',
            $_POST['ban'] ? '1' : '0',
            '1',
            $_POST['system'] ? '1' : '0',
            $_POST['teacher'] ? '1' : '0',
            $_POST['dancer'] ? '1' : '0'
        );
        \Redirect::to($_POST['returnURI'] ?? '/admin/users');
    }

    public static function edit($id)
    {
        \Permissions::checkError('users', P_ADMIN);
        if (!$data = \DBUser::getUserData($id)) {
            \Message::warning('Uživatel s takovým ID neexistuje');
            \Redirect::to($_POST['returnURI'] ?? '/admin/users');
        }
        if (!$data['u_confirmed']) {
            \Message::warning('Uživatel "' . $data['u_login'] . '" ještě není potvrzený');
            \Redirect::to($_POST['returnURI'] ?? '/admin/users');
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

    public static function editPost($id)
    {
        \Permissions::checkError('users', P_ADMIN);
        if (!$data = \DBUser::getUserData($id)) {
            \Message::warning('Uživatel s takovým ID neexistuje');
            \Redirect::to($_POST['returnURI'] ?? '/admin/users');
        }
        if (!$data['u_confirmed']) {
            \Message::warning('Uživatel "' . $data['u_login'] . '" ještě není potvrzený');
            \Redirect::to($_POST['returnURI'] ?? '/admin/users');
        }
        $form = self::checkData('edit');
        if (!$form->isValid()) {
            \Message::warning($form->getMessages());
            return self::displayForm('edit');
        }
        \Database::query(
            "UPDATE users SET u_jmeno='?',u_prijmeni='?',u_pohlavi='?',u_email='?'," .
            "u_telefon='?',u_narozeni='?',u_rodne_cislo='?', u_poznamky='?',u_street='?',u_conscription_number='?'," .
            "u_orientation_number='?',u_district='?',u_city='?',u_postal_code='?'," .
            "u_nationality='?',u_group='?',u_skupina='?',u_lock='?',u_ban='?',u_system='?',u_dancer='?'" .
            " WHERE u_id='?'",
            $_POST['jmeno'],
            $_POST['prijmeni'],
            $_POST['pohlavi'],
            $_POST['email'],
            $_POST['telefon'],
            (string) new \Date($_POST['narozeni'] ?? null),
            $_POST['rodnecislo'],
            $_POST['poznamky'],
            $_POST['street'],
            $_POST['popisne'],
            $_POST['orientacni'],
            $_POST['district'],
            $_POST['city'],
            $_POST['postal'],
            $_POST['nationality'],
            $_POST['group'],
            $_POST['skupina'],
            $_POST['lock'] ? 1 : 0,
            $_POST['ban'] ? 1 : 0,
            $_POST['system'] ? 1 : 0,
            $_POST['dancer'] ? 1 : 0,
            $_POST['teacher'] ? 1 : 0,
            $id,
        );
        \Redirect::to($_POST['returnURI'] ?? '/admin/users');
    }

    public static function unconfirmed()
    {
        \Permissions::checkError('users', P_ADMIN);
        \Render::twig('Admin/UsersUnconfirmed.twig', [
            'groups' => \Database::queryArray("SELECT * FROM permissions"),
            'skupiny' => \DBSkupiny::get(),
            'data' => \DBUser::getNewUsers(),
        ]);
    }

    public static function unconfirmedPost()
    {
        \Permissions::checkError('users', P_ADMIN);
        $id = $_POST['confirm'];
        if (!\DBUser::getUser($id)) {
            \Message::warning('Uživatel s takovým ID neexistuje');
            \Redirect::to($_POST['returnURI'] ?? '/admin/users');
        }
        \Database::query("select confirm_user('?', '?', '?')", $id, $_POST[$id . '-group'], $_POST[$id . '-skupina']);
        \Redirect::to('/admin/users/unconfirmed');
    }

    private static function displayForm($action)
    {
        \Render::twig('Admin/UsersForm.twig', [
            'action' => $action,
            'returnURI' => $_POST['returnURI'] ?? ($_SERVER['HTTP_REFERER'] ?? '/admin/users'),
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
