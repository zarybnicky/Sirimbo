<?php
require_once 'files/Controller/Admin.php';
class Controller_Admin_Permissions extends Controller_Admin
{
    public function __construct()
    {
        Permissions::checkError('permissions', P_ADMIN);
    }

    public function view($request)
    {
        switch($request->post('action')) {
            case 'edit':
                $data = $request->post('permissions');
                if ($data[0]) {
                    $this->redirect('/admin/permissions/edit/' . $data[0]);
                }
                break;
            case 'remove':
                if (!is_array($request->post('permissions'))) {
                    break;
                }
                $this->redirect(
                    '/admin/permissions/remove?' .
                    http_build_query(['u' => $request->post('permissions')])
                );
                break;
        }
        $data = array_map(
            function ($item) {
                return [
                    'checkBox' => $this->checkbox('permissions[]', $item['pe_id']),
                    'name' => $item['pe_name'],
                    'description' => $item['pe_description']
                ];
            },
            DBPermissions::getGroups()
        );
        $this->render(
            'files/View/Admin/Permissions/Overview.inc',
            ['showMenu' => !TISK, 'data' => $data]
        );
    }

    public function add($request)
    {
        if (!$request->post()) {
            $this->renderForm($request);
            return;
        }
        if (is_object($f = $this->checkData($request))) {
            $this->redirect()->setMessage($f->getMessages());
            $this->renderForm($request);
            return;
        }

        $permissions = [];
        foreach (array_keys(Settings::$permissions) as $name) {
            $permissions[$name] = $request->post($name);
        }
        DBPermissions::addGroup(
            $request->post('name'),
            $request->post('description'),
            $permissions
        );

        $this->redirect(
            $request->post('referer') ?: '/admin/permissions',
            'Úroveň úspěšně přidána'
        );
    }

    public function edit($request)
    {
        $id = $request->getId();
        if (!$id || !($data = DBPermissions::getSingleGroup($id))) {
            $this->redirect(
                $request->post('referer') ?: '/admin/permissions',
                'Skupina s takovým ID neexistuje'
            );
        }

        if (!$request->post()) {
            $this->renderForm($request, $data);
            return;
        }
        if (is_object($f = $this->checkData($request))) {
            $this->redirect()->setMessage($f->getMessages());
            $this->renderForm($request, $data);
            return;
        }

        $permissions = [];
        foreach (array_keys(Settings::$permissions) as $name) {
            $permissions[$name] = $request->post($name);
        }
        DBPermissions::editGroup(
            $id,
            $request->post('name'),
            $request->post('description'),
            $permissions
        );

        $this->redirect(
            $request->post('referer') ?: '/admin/permissions',
            'Oprávnění úspěšně upravena'
        );
    }

    public function remove($request)
    {
        if (!is_array($request->post('data')) && !is_array($request->get('u'))) {
            $this->redirect('/admin/permissions');
        }
        if ($request->post() && $request->post('action') == 'confirm') {
            foreach ($request->post('data') as $id) {
                DBPermissions::removeGroup($id);
            }
            $this->redirect(
                '/admin/permissions',
                'Úrovně odebrány. Nezapomeňte přiřadit uživatelům z těchto skupin jinou skupinu!'
            );
        }
        $data = array_map(
            function ($id) {
                $item = DBPermissions::getSingleGroup($id);
                return ['id' => $item['pe_id'], 'text' => $item['pe_name']];
            },
            $request->get('u')
        );

        $this->render(
            'files/View/Admin/RemovePrompt.inc',
            [
                'header' => 'Správa oprávnění',
                'prompt' =>
                    $this->notice('Uživatelům z těchto skupin bude nutmné přiřadit jinou skupinu!')
                    . 'Opravdu chcete odstranit uživatelské úrovně:',
                'returnURI' => $request->getReferer(),
                'data' => $data
            ]
        );
    }

    protected function renderForm($request, $data = null)
    {
        if (!$request->post()) {
            foreach (Settings::$permissions as $name => $item) {
                $request->post($name, $data['pe_' . $name]);
            }
        }
        $settings = array_map(
            function ($name, $item) use ($request, $data) {
                $value = $request->post($name) ?: $data ? $data['pe_' . $name] : $item['default'];
                return [
                    'name' => $item['name'],
                    'value' => $value,
                    'items' => array_map(
                        function ($key, $levelName) use ($name, $item, $value) {
                            return isset($item[$key])
                                ? $this->radio($name, $key)
                                       ->set($key == $value)
                                       ->label($levelName)
                                : '';
                        },
                        array_keys(Settings::$permissionLevels),
                        array_values(Settings::$permissionLevels)
                    )
                ];
            },
            array_keys(Settings::$permissions),
            array_values(Settings::$permissions)
        );

        $this->render(
            'files/View/Admin/Permissions/Form.inc',
            [
                'action' => $request->getAction(),
                'name' => $request->post('name') ?: $data ? $data['pe_name'] : '',
                'description' => $request->post('description') ?: $data ? $data['pe_description'] : '',
                'settings' => $settings
            ]
        );
    }

    private function checkData($request)
    {
        $f = new Form();
        foreach (Settings::$permissions as $name => $item) {
            $f->checkArrayKey(
                $request->post($name), $item,
                'Neplatná hodnota práva "' . $name . '"', $name
            );
        }
        return $f->isValid() ? true : $f;
    }
}
