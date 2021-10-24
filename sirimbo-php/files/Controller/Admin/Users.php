<?php
namespace Olymp\Controller\Admin;

class Users
{
    public static function list()
    {
        \Permissions::checkError('users', P_ADMIN);

        $groupOptions = ['all' => 'všechna'];
        foreach (\DBPermissions::getGroups() as $row) {
            $groupOptions[$row['pe_id']] = $row['pe_name'];
        }
        $skupinyOptions = ['all' => 'všechny'];
        foreach (\DBSkupiny::get() as $item) {
            $skupinyOptions[$item['s_id']] = $item['s_name'];
        }

        $sortOptions = [
            'prijmeni' => 'přijmení',
            'narozeni' => 'data narození',
            'var-symbol' => 'var. symbolu'
        ];
        $statusOptions = [
            'all' => 'všichni',
            'system' => 'systémoví',
            'ban' => 'zablokovaní'
        ];

        $options['group'] = $_GET['group'] ?? 'all';
        $options['skupina'] = $_GET['skupina'] ?? 'all';
        $options['sort'] = $_GET['sort'] ?? 'prijmeni';
        $options['status'] = $_GET['status'] ?? 'all';
        if (!in_array($options['group'], array_keys($groupOptions))) {
            $options['group'] = 'all';
        }
        if (!in_array($options['skupina'], array_keys($skupinyOptions))) {
            $options['skupina'] = 'all';
        }
        if (!in_array($options['sort'], array_keys($sortOptions))) {
            $options['sort'] = 'prijmeni';
        }
        if (!in_array($options['status'], array_keys($statusOptions))) {
            $options['status'] = 'all';
        }

        $pager = new \Paging(new \DBUser(), $options);
        $pager->setCurrentPage($_GET['p'] ?? null);
        $pager->setItemsPerPage($_GET['c'] ?? null);

        $action = $_GET['view'] ?? 'info';
        $copySkupinyOptions = $skupinyOptions;
        unset($copySkupinyOptions['all']);

        \Render::twig('Admin/Users.twig', [
            'groupOptions' => $groupOptions,
            'skupinyOptions' => $skupinyOptions,
            'onlySkupinyOptions' => $copySkupinyOptions,
            'sortOptions' => $sortOptions,
            'statusOptions' => $statusOptions,
            'startIndex' => $pager->getItemsPerPage() * ($pager->getCurrentPage() - 1),
            'navigation' => $pager->getNavigation(),
            'view' => $action,
            'status' => $_GET['status'] ?? '',
            'skupina' => $_GET['skupina'] ?? '',
            'group' => $_GET['group'] ?? '',
            'sort' => $_GET['sort'] ?? '',
            'data' => array_for($pager->getItems(), fn($item) => [
                'user' => \User::fromArray($item),
                'skupina' => [
                    'id' => $item['s_id'],
                    'description' => $item['s_description'],
                    'color' => $item['s_color_rgb'],
                ],
                'groupInfo' => $groupOptions[$item['u_group']] ?? '',
            ]),
        ]);
    }

    public static function listPost()
    {
        \Permissions::checkError('users', P_ADMIN);
        foreach ($_POST['save'] as $userId) {
            if (!$user = \DBUser::getUser($userId)) {
                continue;
            }
            if (((bool) $_POST[$userId . '-system']) !== ((bool) $user->getSystem())
                || ((bool) $_POST[$userId . '-ban']) !== ((bool) $user->getBanned())
                || ($_POST[$userId . '-skupina'] != $user->getPermissionGroup())
            ) {
                \DBUser::setUserData(
                    $userId,
                    $user->getName(),
                    $user->getSurname(),
                    $user->getGender(),
                    $user->getEmail(),
                    $user->getPhone(),
                    $user->getBirthDate(),
                    $user->getBirthNumber(),
                    $user->getNotes(),
                    $user->getStreet(),
                    $user->getConscriptionNumber(),
                    $user->getOrientationNumber(),
                    $user->getDistrict(),
                    $user->getCity(),
                    $user->getPostalCode(),
                    $user->getNationality(),
                    $user->getTrainingGroup(),
                    $_POST[$userId . '-skupina'],
                    $user->getLocked() ? '1' : '0',
                    $_POST[$userId . '-ban'] ? '1' : '0',
                    $_POST[$userId . '-system'] ? '1' : '0',
                    $user->getDancer() ? '1' : '0',
                    $user->getTeacher() ? '1' : '0',
                    $user->getMemberSince(),
                    $user->getMemberUntil(),
                    $user->getGdprSignedAt()
                );
            }
        }
        \Redirect::to('/admin/users');
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
        \DBUser::removeUser($id);
        \Redirect::to($_POST['returnURI'] ?? '/admin/users');
    }

    public static function add()
    {
        \Permissions::checkError('users', P_ADMIN);
        return static::displayForm('add');
    }

    public static function addPost()
    {
        \Permissions::checkError('users', P_ADMIN);
        $form = static::checkData('add');
        if (!$form->isValid()) {
            \Message::warning($form->getMessages());
            return static::displayForm('add');
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
        return static::displayForm('edit');
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
        $form = static::checkData('edit');
        if (!$form->isValid()) {
            \Message::warning($form->getMessages());
            return static::displayForm('edit');
        }
        \DBUser::setUserData(
            $id,
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
            $data['u_member_since'],
            $data['u_member_until'],
            $data['u_gdpr_signed_at']
        );
        \Redirect::to($_POST['returnURI'] ?? '/admin/users');
    }

    public static function signAs($id)
    {
        \Permissions::checkError('users', P_ADMIN);
        if (!\DBUser::getUserData($id)) {
            \Message::warning('Uživatel s takovým ID neexistuje');
            \Redirect::to($_POST['returnURI'] ?? '/admin/users');
        }
        \Session::loadUser($id);
        \Redirect::to('/');
    }

    public static function getMsmtCsv()
    {
        \Permissions::checkError('users', P_OWNED);

        $out = implode(';', [
            'JMENO',
            'DALSI_JMENA',
            'PRIJMENI',
            'DATUM_NAROZENI',

            'NAZEV_OBCE',
            'NAZEV_CASTI_OBCE',
            'NAZEV_ULICE',
            'CISLO_POPISNE',
            'CISLO_ORIENTACNI',
            'PSC',

            'STRECHA',
            'SVAZ',
            'KLUB',
            'ODDIL',

            'DRUH_SPORTU',
            'SPORTOVEC',
            'TRENER',
            'CLENSTVI_OD',
            'CLENSTVI_DO',
            'OBCANSTVI',
            'EXT_ID'
        ]);

        $oldest = \DBPlatby::getOldestPayment();
        $newest = \DBPlatby::getNewestPayment();
        foreach (\DBUser::getUsers() as $u) {
            if ($u['u_ban'] || !$u['u_confirmed'] || $u['u_system']) {
                continue;
            }
            // skupina - ne Host/VIP
            if (in_array($u['u_skupina'], ['9', '10', '13'])) {
                continue;
            }
            // od 1.9.2019
            if (new \DateTime($newest[$u['u_id']]) < new \DateTime('2019-09-01')) {
                continue;
            }

            $out .= '
' . implode(';', [
                $u['u_jmeno'],
                '',
                $u['u_prijmeni'],
                implode('.', array_reverse(explode('-', $u['u_narozeni']))),
                $u['u_city'],
                $u['u_district'],
                $u['u_street'],
                $u['u_conscription_number'],
                $u['u_orientation_number'],
                str_replace(' ', '', $u['u_postal_code']),
                '',
                '',
                '',
                '',
                '66',
                $u['u_dancer'] ? '1' : '0',
                $u['u_teacher'] ? '1' : '0',
                isset($oldest[$u['u_id']])
                ? implode('.', array_reverse(explode('-', $oldest[$u['u_id']])))
                : '',
                isset($newest[$u['u_id']])
                ? implode('.', array_reverse(explode('-', $newest[$u['u_id']])))
                : '',
                $u['u_nationality'],
                ''
            ]);
        }

        header('Pragma: no-cache');
        header('Content-Type: text/csv');
        header('Content-Disposition: inline; filename="olymp-msmt-export.csv');
        echo chr(239) . chr(187) . chr(191) . $out;
    }

    public static function unconfirmed()
    {
        \Permissions::checkError('users', P_ADMIN);
        \Render::twig('Admin/UsersUnconfirmed.twig', [
            'groups' => \DBPermissions::getGroups(),
            'skupiny' => \DBSkupiny::get(),
            'data' => \DBUser::getNewUsers(),
        ]);
    }

    public static function unconfirmedPost()
    {
        \Permissions::checkError('users', P_ADMIN);
        $id = $_POST['confirm'];
        if (!$data = \DBUser::getUser($id)) {
            \Message::warning('Uživatel s takovým ID neexistuje');
            \Redirect::to($_POST['returnURI'] ?? '/admin/users');
        }
        \DBUser::confirmUser($id, $_POST[$id . '-group'], $_POST[$id . '-skupina']);
        \Mailer::registrationConfirmNotice($data->getEmail(), $data->getLogin());
        \Redirect::to('/admin/users/unconfirmed');
    }

    public static function duplicate()
    {
        \Permissions::checkError('users', P_ADMIN);
        \Render::twig('Admin/UsersDuplicate.twig', [
            'data' => \DBUser::getDuplicateUsers(),
        ]);
    }

    public static function statistiky()
    {
        \Permissions::checkError('users', P_ADMIN);
        \Render::twig('Admin/UsersStatistics.twig', [
            'total' => count(\DBUser::getUsers()),
            'active' => count(\DBUser::getActiveUsers()),
            'groups' => \DBUser::getGroupCounts(),
        ]);
    }

    private static function displayForm($action)
    {
        \Render::twig('Admin/UsersForm.twig', [
            'action' => $action,
            'returnURI' => $_POST['returnURI'] ?? ($_SERVER['HTTP_REFERER'] ?? '/admin/users'),
            'countries' => \Countries::$countries,
            'groups' => \DBPermissions::getGroups(),
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
