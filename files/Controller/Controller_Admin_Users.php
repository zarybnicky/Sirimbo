<?php
class Controller_Admin_Users extends Controller_Admin
{
    public function __construct()
    {
        parent::__construct();
        Permissions::checkError('users', P_OWNED);
    }

    public function view($request)
    {
        Permissions::checkError('users', P_ADMIN);
        if ($request->post('action') == 'save') {
            foreach ($request->post('save') as $user_id) {
                $user = DBUser::getUserData($user_id);
                if (((bool) $request->post($user_id . '-system')) !== ((bool) $user['u_system'])
                    || ((bool) $request->post($user_id . '-ban')) !== ((bool) $user['u_ban'])
                    || ($request->post($user_id . '-skupina') != $user['u_skupina'])
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
                        $user['u_lock'] ? '1' : '0',
                        $request->post($user_id . '-ban') ? '1' : '0',
                        $request->post($user_id . '-system') ? '1' : '0'
                    );
                }
            }
        }
        $this->displayOverview($request);
    }

    public function remove($request)
    {
        Permissions::checkError('users', P_ADMIN);
        if (!$request->getId()) {
            $this->redirect($request->post('returnURI') ?: '/admin/users');
        }
        $id = $request->getId();

        if ($request->post('action') == 'confirm') {
            DBUser::removeUser($id);
            $this->redirect($request->post('returnURI') ?: '/admin/users');
        }

        $item = DBUser::getUserData($id);
        $this->render('files/View/Admin/RemovePrompt.inc', [
            'header' => 'Správa uživatelů',
            'prompt' => 'Opravdu chcete odstranit uživatele:',
            'returnURI' => $request->getReferer() ?: '/admin/users',
            'data' => [[
                'id' => $item['u_id'],
                'text' => $item['u_jmeno'] . ' ' . $item['u_prijmeni'] . ' - ' . $item['u_login']
            ]]
        ]);
    }

    public function add($request)
    {
        Permissions::checkError('users', P_ADMIN);
        if (!$request->post() || is_object($f = $this->checkData($request, 'add'))) {
            if ($request->post()) {
                $this->redirect()->warning($f->getMessages());
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
            $request->post('ulice'),
            $request->post('popisne'),
            $request->post('orientacni'),
            $request->post('district'),
            $request->post('city'),
            $request->post('postal'),
            $request->post('nationality'),
            $request->post('group'),
            $request->post('skupina'),
            $request->post('lock') ? '1' : '0',
            $request->post('ban') ? '1' : '0',
            '1',
            $request->post('system') ? '1' : '0',
            $request->post('trener') ? '1' : '0',
            $request->post('dancer') ? '1' : '0'
        );
        $this->redirect($request->post('returnURI') ?: '/admin/users');
    }

    public function edit($request)
    {
        Permissions::checkError('users', P_ADMIN);
        $id = $request->getId();
        if (!$id || !($data = DBUser::getUserData($id))) {
            $this->redirect()->warning('Uživatel s takovým ID neexistuje');
            $this->redirect($request->post('returnURI') ?: '/admin/users');
        }
        if (!$data['u_confirmed']) {
            $this->redirect()->warning('Uživatel "' . $data['u_login'] . '" ještě není potvrzený');
            $this->redirect($request->post('returnURI') ?: '/admin/users');
        }

        if (!$request->post() || is_object($f = $this->checkData($request, 'edit'))) {
            if (!$request->post()) {
                $request->post('login', $data['u_login']);
                $request->post('group', $data['u_group']);
                $request->post('ban', $data['u_ban']);
                $request->post('lock', $data['u_lock']);
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
                $this->redirect()->warning($f->getMessages());
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
            $request->post('lock') ? 1 : 0,
            $request->post('ban') ? 1 : 0,
            $request->post('system') ? 1 : 0
        );
        $this->redirect($request->post('returnURI') ?: '/admin/users');
    }

    public function unconfirmed($request)
    {
        Permissions::checkError('users', P_ADMIN);
        if ($request->post('confirm')) {
            $id = $request->post('confirm');
            $data = DBUser::getUserData($id);

            DBUser::confirmUser(
                $id,
                $request->post($id . '-group'),
                $request->post($id . '-skupina')
            );
            Mailer::registrationConfirmNotice($data['u_email'], $data['u_login']);
            $this->redirect('/admin/users/unconfirmed');
        }

        $users = DBUser::getNewUsers();
        if (empty($users)) {
            $this->render('files/View/Empty.inc', [
                'header' => 'Správa uživatelů',
                'notice' => 'Žádní nepotvrzení uživatelé nejsou v databázi.'
            ]);
            return;
        }
        $groups = DBPermissions::getGroups();
        $s_group = $this->select()->optionsAssoc($groups, 'pe_id', 'pe_name')->set(3);

        $skupiny = DBSkupiny::get();
        $s_skupina = new SelectHelper();
        $s_skupina->select()->optionsAssoc($skupiny, 's_id', 's_name');

        $users = array_map(
            function ($item) use ($s_group, $s_skupina) {
                return [
                    'id' => $item['u_id'],
                    'buttons' => (
                        '<button class="a" name="confirm" value="' . $item['u_id'] . '">&#10003;</button>' .
                        '&nbsp;' .
                        $this->removeLink('/admin/users/remove/' . $item['u_id'])
                    ),
                    'group' => $s_group->name($item['u_id'] . '-group')->render(),
                    'skupina' => $s_skupina->name($item['u_id'] . '-skupina')->set($item['u_skupina'])->render(),
                    'fullName' => $item['u_jmeno'] . ' ' . $item['u_prijmeni'],
                    'narozeni' => formatDate($item['u_narozeni']),
                    'poznamky' => $item['u_poznamky']
                ];
            },
            $users
        );

        $this->render('files/View/Admin/Users/Unconfirmed.inc', [
            'header' => 'Správa uživatelů',
            'subheader' => 'Nepotvrzení uživatelé',
            'data' => $users
        ]);
    }

    public function duplicate($request)
    {
        Permissions::checkError('users', P_ADMIN);
        $users = array_map(
            function ($item) {
                return [
                    'id' => $item['u_id'],
                    'buttons' => $this->removeLink('/admin/users/remove/' . $item['u_id']),
                    'fullName' => $this->colorbox($item['s_color_rgb'], $item['s_description'])
                        . '&nbsp;' . $item['u_prijmeni'] . ', ' . $item['u_jmeno'],
                    'email' => $item['u_email'],
                    'telefon' => $item['u_telefon'],
                    'narozeni' => formatDate($item['u_narozeni']),
                    'timestamp' => formatTimestamp($item['u_timestamp'])
                ];
            },
            DBUser::getDuplicateUsers()
        );
        $this->render('files/View/Admin/Users/Duplicate.inc', [
            'header' => 'Správa uživatelů',
            'subheader' => 'Duplicitní uživatelé',
            'data' => $users
        ]);
    }

    public function statistiky($request)
    {
        Permissions::checkError('users', P_ADMIN);

        $data = array_map(
            function ($item) {
                return ['group' => $item['pe_name'], 'count' => $item['count']];
            },
            DBUser::getGroupCounts()
        );

        $all = DBUser::getUsers();
        $active = DBUser::getActiveUsers();
        array_unshift(
            $data,
            ['group' => 'Uživatelé v databázi', 'count' => count($all)],
            ['group' => 'Aktivní uživatelé', 'count' => count($active)]
        );

        $this->render('files/View/Admin/Users/Statistiky.inc', [
            'header' => 'Správa uživatelů',
            'subheader' => 'Statistiky',
            'data' => $data
        ]);
    }

    private function displayOverview($request)
    {
        $groupOptions = ['all' => 'všechna'];
        foreach (DBPermissions::getGroups() as $row) {
            $groupOptions[$row['pe_id']] = $row['pe_name'];
        }

        $skupinyOptions = ['all' => 'všechny'];
        foreach (DBSkupiny::get() as $item) {
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
            'ban' => 'zabanovaní'
        ];

        $options['group'] = in_array($request->get('group'), array_keys($groupOptions))
                          ? $request->get('group')
                          : 'all';
        $options['skupina'] = in_array($request->get('skupina'), array_keys($skupinyOptions))
                            ? $request->get('skupina')
                            : 'all';
        $options['sort'] = in_array($request->get('sort'), array_keys($sortOptions))
                         ? $request->get('sort')
                         : 'prijmeni';
        $options['status'] = in_array($request->get('status'), array_keys($statusOptions))
                           ? $request->get('status')
                           : 'all';

        $pager = new Paging('DBUser', $options);
        $pager->setCurrentPage($request->get('p'));
        $pager->setItemsPerPage($request->get('c'));

        $i = $pager->getItemsPerPage() * ($pager->getCurrentPage() - 1);

        $action = $request->get('view') ?: 'info';
        if ($action == 'status') {
            $skupinySelect = $this->select()->options($skupinyOptions);
        } else {
            $skupinySelect = null;
        }

        $data = array_map(
            function ($item) use ($action, $groupOptions, &$i, $skupinySelect) {
                $out = [
                    'checkBox' => $this->editLink('/admin/users/edit/' . $item['u_id'])
                    . '&nbsp;' . $this->removeLink('/admin/users/remove/' . $item['u_id']),
                    'index' => ++$i . '. ',
                    'varSymbol' => User::varSymbol($item['u_id']),
                    'fullName' => $item['u_prijmeni'] . ', ' . $item['u_jmeno'],
                    'birthDate' => formatDate($item['u_narozeni']),
                    'colorBox' => $this->colorbox($item['s_color_rgb'], $item['s_description'])
                                        ->render(),
                    'groupInfo' => $groupOptions[$item['u_group']]
                ];
                if ($action == 'status') {
                    $out['skupina'] = (
                        $this->hidden('save[]', $item['u_id'])
                        . $skupinySelect->name($item['u_id'] . '-skupina')
                                        ->set($item['u_skupina'])
                    );
                    $out['system'] = $this->checkbox($item['u_id'] . '-system', '1')
                                          ->set($item['u_system'])->render();
                    $out['ban'] = $this->checkbox($item['u_id'] . '-ban', '1')
                                       ->set($item['u_ban'])->render();
                }
                return $out;
            },
            $pager->getItems()
        );

        $this->render('files/View/Admin/Users/Overview.inc', [
            'header' => 'Správa uživatelů',
            'groupOptions' => $groupOptions,
            'skupinyOptions' => $skupinyOptions,
            'sortOptions' => $sortOptions,
            'statusOptions' => $statusOptions,
            'data' => $data,
            'navigation' => $pager->getNavigation($request->get()),
            'view' => $action,
            'status' => $request->get('status') ?: '',
            'skupina' => $request->get('skupina') ?: '',
            'group' => $request->get('group') ?: '',
            'sort' => $request->get('sort') ?: ''
        ]);
        return;
    }

    private function displayForm($request)
    {
        $groups = array_map(
            function ($item) {
                return ['id' => $item['pe_id'], 'name' => $item['pe_name']];
            },
            DBPermissions::getGroups()
        );
        $skupiny = array_map(
            function ($item) {
                return [
                    'id' => $item['s_id'],
                    'color' => $item['s_color_rgb'],
                    'popis' => $item['s_name']
                ];
            },
            DBSkupiny::get()
        );
        $this->render('files/View/Admin/Users/Form.inc', [
            'header' => 'Správa uživatelů',
            'subheader' => ($request->getAction() == 'add' ? 'Přidat' : 'Upravit') . ' uživatele',
            'action' => $request->getAction(),
            'returnURI' => $request->post('returnURI') ?: ($request->getReferer() ?: '/admin/users'),
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
            'system' => $request->post('system') ?: '',
            'group' => $request->post('group') ?: '',
            'skupina' => $request->post('skupina') ?: '',
            'dancer' => $request->post('dancer') ?: '',
            'trener' => $request->post('trener') ?: ''
        ]);
    }

    private function checkData($request, $action = 'add')
    {
        $narozeni = $this->date('narozeni')->getPost($request);

        $f = new Form();
        $f->checkDate($narozeni, 'Neplatné datum narození', 'narozeni');
        $f->checkInArray($request->post('pohlavi'), ['m', 'f'], 'Neplatné pohlaví', 'pohlavi');
        $f->checkEmail($request->post('email'), 'Neplatný formát emailu', 'email');
        $f->checkPhone($request->post('telefon'), 'Neplatný formát telefoního čísla', 'telefon');

        if ($action == 'add') {
            $f->checkLogin($request->post('login'), 'Špatný formát přihlašovacího jména', 'login');
            $f->checkPassword($request->post('pass'), 'Špatný formát hesla', 'pass');
        }
        return $f->isValid() ? true : $f;
    }
}