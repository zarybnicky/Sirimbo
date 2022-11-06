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
            if (isset($newest[$u['u_id']]) && new \DateTime($newest[$u['u_id']]) < new \DateTime('2019-09-01')) {
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
}
