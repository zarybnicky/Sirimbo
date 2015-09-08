<?php
require_once 'files/Controller/Admin.php';
class Controller_Admin_Users extends Controller_Admin
{
    public function __construct()
    {
        Permissions::checkError('users', P_OWNED);
    }
    public function view($request)
    {
        switch($request->post('action')) {
        case 'edit':
            $users = $request->post('users');
            if ($users[0]) {
                $this->redirect('/admin/users/edit/' . $users[0]);
            }
            break;
        case 'platby':
            $users = $request->post('users');
            if ($users[0]) {
                $this->redirect('/admin/users/platby/' . $users[0]);
            }
            break;
        case 'remove':
            if (!is_array($request->post('users'))) {
                break;
            }
            $this->redirect('/admin/users/remove?' . http_build_query(array('u' => $request->post('users'))));
            break;
        case 'save':
            Permissions::checkError('users', P_ADMIN);
            foreach ($request->post('save') as $user_id) {
                $user = DBUser::getUserData($user_id);
                if (((bool) $request->post($user_id . '-dancer')) !== ((bool) $user['u_dancer']) ||
                    ((bool) $request->post($user_id . '-system')) !== ((bool) $user['u_system']) ||
                    ($request->post($user_id . '-skupina') != $user['u_skupina'])
                ) {
                    DBUser::setUserData(
                        $user_id,
                        $user['u_jmeno'],
                        $user['u_prijmeni'],
                        $user['u_pohlavi'],
                        $user['u_email'],
                        $user['u_telefon'],
                        $user['u_narozeni'],
                        $user['u_poznamky'],
                        $user['u_group'],
                        $request->post($user_id . '-skupina'),
                        $request->post($user_id . '-dancer') ? '1' : '0',
                        $user['u_lock'] ? '1' : '0',
                        $user['u_ban'] ? '1' : '0',
                        $request->post($user_id . '-system') ? '1' : '0'
                    );
                }
            }
            break;
        }
        Permissions::checkError('users', P_ADMIN);
        if ($request->get('v') === null) {
            $request->get('v', 'info');
        }
        $this->displayOverview($request, $request->get('v'));
    }

    public function remove($request)
    {
        Permissions::checkError('users', P_ADMIN);
        if (!is_array($request->post('data')) && !is_array($request->get('u'))) {
            $this->redirect('/admin/users');
        }
        if ($request->post() && $request->post('action') == 'confirm') {
            foreach ($request->post('data') as $id) {
                DBUser::removeUser($id);
            }
            $this->redirect('/admin/users', 'Uživatelé odebráni');
        }
        $data = array();
        foreach ($request->get('u') as $id) {
            $item = DBUser::getUserData($id);
            $data[] = array(
                'id' => $item['u_id'],
                'text' => $item['u_jmeno'] . ' ' . $item['u_prijmeni'] . ' - ' .
                    $item['u_login']
            );
        }
        $this->render(
            'files/View/Admin/RemovePrompt.inc',
            array(
                'header' => 'Správa uživatelů',
                'prompt' => 'Opravdu chcete odstranit uživatele:',
                'returnURI' => $request->getReferer(),
                'data' => $data
            )
        );
    }

    public function add($request)
    {
        Permissions::checkError('users', P_ADMIN);
        if (!$request->post() || is_object($f = $this->checkData($request, 'add'))) {
            if ($request->post()) {
                $this->redirect()->setMessage($f->getMessages());
            }
            $this->displayForm($request);
            return;
        }

        DBUser::addUser(
            strtolower($request->post('login')),
            User::crypt($request->post('pass')),
            $request->post('jmeno'),
            $request->post('prijmeni'),
            $request->post('pohlavi'),
            $request->post('email'),
            $request->post('telefon'),
            (string) $this->date('narozeni')->getPost($request),
            $request->post('poznamky'),
            $request->post('group'),
            $request->post('skupina'),
            $request->post('dancer') ? '1' : '0',
            $request->post('lock') ? '1' : '0',
            $request->post('ban') ? '1' : '0',
            '1',
            $request->post('system') ? '1' : '0'
        );
        $this->redirect('/admin/users', 'Uživatel úspěšně přidán');
    }

    public function edit($request)
    {
        Permissions::checkError('users', P_ADMIN);
        $id = $request->getId();
        if (!$id || !($data = DBUser::getUserData($id))) {
            $this->redirect(
                '/admin/users',
                'Uživatel s takovým ID neexistuje'
            );
        }
        if (!$data['u_confirmed']) {
            $this->redirect(
                '/admin/users',
                'Uživatel "' . $data['u_login'] . '" ještě není potvrzený'
            );
        }

        if (!$request->post() || is_object($f = $this->checkData($request, 'edit'))) {
            if (!$request->post()) {
                $request->post('login', $data['u_login']);
                $request->post('group', $data['u_group']);
                $request->post('ban', $data['u_ban']);
                $request->post('lock', $data['u_lock']);
                $request->post('dancer', $data['u_dancer']);
                $request->post('system', $data['u_system']);
                $request->post('jmeno', $data['u_jmeno']);
                $request->post('prijmeni', $data['u_prijmeni']);
                $request->post('pohlavi', $data['u_pohlavi']);
                $request->post('email', $data['u_email']);
                $request->post('telefon', $data['u_telefon']);
                $request->post('narozeni', $data['u_narozeni']);
                $request->post('skupina', $data['u_skupina']);
                $request->post('poznamky', $data['u_poznamky']);
            } else {
                $this->redirect()->setMessage($f->getMessages());
            }
            $this->displayForm($request);
            return;
        }
        $narozeni = $this->date('narozeni')->getPost($request);
        DBUser::setUserData(
            $id,
            $request->post('jmeno'),
            $request->post('prijmeni'),
            $request->post('pohlavi'),
            $request->post('email'),
            $request->post('telefon'),
            (string) $narozeni,
            $request->post('poznamky'),
            $request->post('group'),
            $request->post('skupina'),
            $request->post('dancer') ? 1 : 0,
            $request->post('lock') ? 1 : 0,
            $request->post('ban') ? 1 : 0,
            $request->post('system') ? 1 : 0
        );
        $this->redirect('/admin/users', 'Uživatel úspěšně upraven');
    }

    public function platby($request)
    {
        Permissions::checkError('users', P_ADMIN);
        $id = $request->getId();
        if (!$id || !($data = DBUser::getUserData($id))) {
            $this->redirect('/admin/users', 'Uživatel s takovým ID neexistuje');
        }
        if (!$data['u_confirmed']) {
            $this->redirect('/admin/users', 'Uživatel "' . $data['u_login'] . '" ještě není potvrzený');
        }

        $this->render('files/View/Admin/Users/Platby.inc');
    }

    public function unconfirmed($request)
    {
        Permissions::checkError('users', P_ADMIN);
        if (!$request->post() || !is_array($request->post('users'))) {
            $users = DBUser::getNewUsers();
            if (empty($users)) {
                $this->render(
                    'files/View/Empty.inc',
                    array(
                        'nadpis' => 'Správa uživatelů',
                        'notice' => 'Žádní nepotvrzení uživatelé nejsou v databázi.'
                    )
                );
            }
            $groups = DBPermissions::getGroups();
            $s_group = $this->select();
            foreach ($groups as $group) {
                $s_group->option($group['pe_id'], $group['pe_name']);
            }

            $skupiny = DBSkupiny::get();
            $s_skupina = new SelectHelper();
            $s_skupina->select();
            foreach ($skupiny as $skupina) {
                $s_skupina->option($skupina['s_id'], $skupina['s_name']);
            }

            $users = array_map(
                function ($item) use ($s_group, $s_skupina) {
                    return array(
                        'id' => $item['u_id'],
                        'checkBox' => $this->checkbox('users[]', $item['u_id'])->render(),
                        'group' => $s_group->name($item['u_id'] . '-group')->render(),
                        'skupina' => $s_skupina->name($item['u_id'] . '-skupina')->render(),
                        'dancer' => $this->checkbox($item['u_id'] . '-dancer', 'dancer')->render(),
                        'fullName' => $item['u_jmeno'] . ' ' . $item['u_prijmeni'],
                        'narozeni' => formatDate($item['u_narozeni']),
                        'poznamky' => $item['u_poznamky']
                    );
                },
                $users
            );

            $this->render(
                'files/View/Admin/Users/Unconfirmed.inc',
                array('data' => $users)
            );
            return;
        }
        if ($request->post('action') == 'confirm') {
            foreach ($request->post('users') as $id) {
                $data = DBUser::getUserData($id);

                DBUser::confirmUser(
                    $id,
                    $request->post($id . '-group'),
                    $request->post($id . '-skupina'),
                    $request->post($id . '-dancer') ? 1 : 0
                );
                Mailer::registrationConfirmNotice($data['u_email'], $data['u_login']);
            }
            $this->redirect('/admin/users', 'Uživatelé potvrzeni');
        } elseif ($request->post('action') == 'remove') {
            $this->redirect('/admin/users/remove?' . http_build_query(array('u' => $request->post('users'))));
        }
    }

    public function duplicate($request)
    {
        Permissions::checkError('users', P_ADMIN);
        if ($request->post() &&
            $request->post('action') == 'remove' &&
            $request->post('users')
        ) {
            $this->redirect(
                '/admin/users/remove?'
                . http_build_query(array('u' => $request->post('users')))
            );
        }

        $users = DBUser::getDuplicateUsers();
        foreach ($users as &$row) {
            $new_data = array(
                'id' => $row['u_id'],
                'checkBox' => $this->checkbox('users[]', $row['u_id'])->render(),
                'colorBox' => $this->colorbox($row['s_color_rgb'], $row['s_description']),
                'fullName' => $row['u_prijmeni'] . ', ' . $row['u_jmeno'],
                'email' => $row['u_email'],
                'telefon' => $row['u_telefon'],
                'narozeni' => formatDate($row['u_narozeni']),
                'timestamp' => formatTimestamp($row['u_timestamp'])
            );
            $row = $new_data;
        }
        $this->render(
            'files/View/Admin/Users/Duplicate.inc',
            array(
                'data' => $users
            )
        );
    }

    public function statistiky($request)
    {
        Permissions::checkError('users', P_ADMIN);
        $all = DBUser::getUsers();
        $active = DBUser::getActiveUsers();
        $dancers = DBUser::getActiveDancers();
        $data = array(
            array('Uživatelé v databázi', count($all)),
            array('Aktivní uživatelé', count($active)),
            array('Aktivní tanečníci', count($dancers))
        );

        $groupcount = DBUser::getGroupCounts();
        foreach ($groupcount as $group) {
            $data[] = array($group['pe_name'], $group['count']);
        }

        foreach ($data as &$row) {
            $new_data = array(
                'group' => $row[0],
                'count' => $row[1]
            );
            $row = $new_data;
        }
        $this->render(
            'files/View/Admin/Users/Statistiky.inc',
            array(
                'data' => $data
            )
        );
    }

    public function temporary($request)
    {
        $type = $request->post('type');
        $jmeno = $request->post('jmeno');
        $prijmeni = $request->post('prijmeni');
        $narozeni = $this->date('narozeni')->getPost($request);

        $login = preg_replace('/[^a-zA-Z0-9.-_]*/', '', strtolower($prijmeni))
               . '_'
               . preg_replace('/[^a-zA-Z0-9.-_]*/', '', strtolower($jmeno));

        if (!($id = DBUser::getUserByFullName($jmeno, $prijmeni)) &&
            !($id = DBUser::getUserID($login))
        ) {
            list($user_id, $par_id) = DBUser::addTemporaryUser($login, $jmeno, $prijmeni, $narozeni);

            header('Content-Type: application/json');
            echo json_encode(
                array(
                    'user_id' => $user_id,
                    'par_id' => $par_id,
                    'jmeno' => $jmeno,
                    'prijmeni' => $prijmeni,
                    'narozeni' => (string) $narozeni,
                    'rok' => $narozeni->getYear()
                )
            );
        } else {
            if (is_array($id)) {
                $id = $id['u_id'];
            }

            $data = DBUser::getUserData($id);
            $partner = DBPary::getLatestPartner($data['u_id'], $data['u_pohlavi']);
            if ($partner && $partner['p_id']) {
                $par_id = $partner['p_id'];
            } else {
                $par_id = DBPary::noPartner($data['u_id']);
            }

            $narozeni = explode('-', $data['u_narozeni']);

            header('Content-Type: application/json');
            echo json_encode(
                array(
                    'user_id' => $data['u_id'],
                    'par_id' => $par_id,
                    'jmeno' => $data['u_jmeno'],
                    'prijmeni' => $data['u_prijmeni'],
                    'narozeni' => $data['u_narozeni'],
                    'rok' => array_shift($narozeni)
                )
            );
        }
        exit;
    }

    private function displayOverview($request, $action)
    {
        $groups = DBPermissions::getGroups();
        $group_lookup = array();
        foreach ($groups as &$row) {
            if ($row['pe_id']) {
                $filter[] = $row['pe_id'];
            }
            $new_data = array(
                'id' => $row['pe_id'],
                'name' => $row['pe_name']
            );
            $group_lookup[$row['pe_id']] = $row['pe_name'];
            $row = $new_data;
        }

        $skupinyselect = $this->select();
        if ($action == 'status') {
            $skupiny = DBSkupiny::get();
            $skupinyselect = $this->select();
            foreach ($skupiny as $skupina) {
                $skupinyselect->option($skupina['s_id'], $skupina['s_name']);
            }
        }

        $sortOptions = array('prijmeni', 'narozeni', 'var-symbol');
        $filterOptions = array_merge(
            array('dancer', 'system', 'all', 'unconfirmed', 'ban'),
            $filter
        );
        $options['sort'] = in_array($request->get('s'), $sortOptions)
                         ? $request->get('s')
                         : 'prijmeni';
        $options['filter'] = in_array($request->get('f'), $filterOptions)
                           ? $request->get('f')
                           : 'all';

        $pager = new Paging(new PagingAdapterDBSelect('DBUser', $options));
        $pager->setCurrentPage($request->get('p'));
        $pager->setItemsPerPage($request->get('c'));
        $pager->setCurrentPageField('p');
        $pager->setItemsPerPageField('c');
        $pager->setDefaultItemsPerPage(20);
        $pager->setPageRange(5);

        $i = $pager->getItemsPerPage() * ($pager->getCurrentPage() - 1);

        $data = array_map(
            function ($item) use ($action, $group_lookup, &$i, $skupinyselect) {
                $out = array(
                    'checkBox'  => $this->checkbox('users[]', $item['u_id'])->render(),
                    'index'     => ++$i . '. ',
                    'varSymbol' => User::varSymbol($item['u_id']),
                    'fullName'  => $item['u_prijmeni'] . ', ' . $item['u_jmeno'],
                    'birthDate' => formatDate($item['u_narozeni']),
                    'colorBox'  => $this->colorbox($item['s_color_rgb'], $item['s_description'])
                                        ->render(),
                    'groupInfo' => $group_lookup[$item['u_group']]
                );
                if ($action == 'status') {
                    $out['skupina'] = (
                        $this->hidden('save[]', $item['u_id'])
                        . $skupinyselect->name($item['u_id'] . '-skupina')
                                        ->set($item['u_skupina'])
                    );
                    $out['dancer'] = $this->checkbox($item['u_id'] . '-dancer', '1')
                                          ->set($item['u_dancer'])->render();
                    $out['system'] = $this->checkbox($item['u_id'] . '-system', '1')
                                          ->set($item['u_system'])->render();
                }
                return $out;
            },
            $pager->getItems()
        );

        $this->render(
            'files/View/Admin/Users/Overview.inc',
            array(
                'showMenu' => !TISK,
                'groups' => $groups,
                'data' => $data,
                'navigation' => $pager->getNavigation($request->get()),
                'view' => $action,
                'f' => $request->get('f') ?: '',
                'v' => $request->get('v') ?: '',
                's' => $request->get('s') ?: ''
            )
        );
        return;
    }

    private function displayForm($request)
    {
        $groups = array_map(
            function ($item) {
                return array(
                    'id' => $item['pe_id'],
                    'name' => $item['pe_name']
                );
            },
            DBPermissions::getGroups()
        );
        $skupiny = array_map(
            function ($item) {
                return array(
                    'id' => $item['s_id'],
                    'color' => $item['s_color_rgb'],
                    'popis' => $item['s_description']
                );
            },
            DBSkupiny::get()
        );
        $this->render(
            'files/View/Admin/Users/Form.inc',
            array(
                'action' => $request->getAction(),
                'groups' => $groups,
                'skupiny' => $skupiny,
                'login' => $request->post('login') ?: '',
                'pass' => $request->post('pass') ?: '',
                'jmeno' => $request->post('jmeno') ?: '',
                'prijmeni' => $request->post('prijmeni') ?: '',
                'pohlavi' => $request->post('pohlavi') ?: '',
                'email' => $request->post('email') ?: '',
                'telefon' => $request->post('telefon') ?: '',
                'narozeni' => $request->post('narozeni') ?: '',
                'poznamky' => $request->post('poznamky') ?: '',
                'lock' => $request->post('lock') ?: '',
                'ban' => $request->post('ban') ?: '',
                'dancer' => $request->post('dancer') ?: '',
                'system' => $request->post('system') ?: '',
                'group' => $request->post('group') ?: '',
                'skupina' => $request->post('skupina') ?: ''
            )
        );
    }

    private function checkData($request, $action = 'add') {
        $narozeni = $this->date('narozeni')->getPost($request);

        $f = new Form();
        $f->checkLength($request->post('jmeno'), 1, 40, 'Špatná délka jména', 'jmeno');
        $f->checkLength($request->post('prijmeni'), 1, 40, 'Špatná délka přijmení', 'prijmeni');
        $f->checkDate($narozeni, 'Neplatné datum narození', 'narozeni');
        $f->checkInArray($request->post('pohlavi'), array('m', 'f'), 'Neplatné pohlaví', 'pohlavi');
        $f->checkEmail($request->post('email'), 'Neplatný formát emailu', 'email');
        $f->checkPhone($request->post('telefon'), 'Neplatný formát telefoního čísla', 'telefon');

        if ($action == 'add') {
            $f->checkLogin($request->post('login'), 'Špatný formát přihlašovacího jména', 'login');
            $f->checkPassword($request->post('pass'), 'Špatný formát hesla', 'pass');
        }
        return $f->isValid() ? true : $f;
    }
}
