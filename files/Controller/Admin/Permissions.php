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
        $data = array_map(
            function ($item) {
                return [
                    'buttons' => $this->editLink('/admin/permissions/edit/' . $item['pe_id'])
                        . '&nbsp;&nbsp;'
                        . $this->removeLink('/admin/permissions/remove/' . $item['pe_id']),
                    'name' => $item['pe_name'],
                    'description' => $item['pe_description']
                ];
            },
            DBPermissions::getGroups()
        );
        $this->render('files/View/Admin/Permissions/Overview.inc', [
            'header' => 'Správa oprávnění',
            'data' => $data
        ]);
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
        if (!$request->getId()) {
            $this->redirect('/admin/permissions');
        }
        $id = $request->getId();

        if ($request->post('action') == 'confirm') {
            DBPermissions::removeGroup($id);
            $this->redirect(
                '/admin/permissions',
                'Úroveň odebrána. Nezapomeňte přiřadit uživatelům z této skupiny jinou skupinu!'
            );
        }

        $item = DBPermissions::getSingleGroup($id);
        $this->render('files/View/Admin/RemovePrompt.inc', [
            'header' => 'Správa oprávnění',
            'prompt' =>
                $this->notice('Uživatelům z této skupiny bude nutné přiřadit jinou skupinu!')
                . 'Opravdu chcete odstranit uživatelskou úroveň:',
            'returnURI' => $request->getReferer() ?: '/admin/permissions',
            'data' => [['id' => $item['pe_id'], 'text' => $item['pe_name']]]
        ]);
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
                $value = $request->post($name) ?: ($data ? $data['pe_' . $name] : $item['default']);
                return [
                    'name' => $item['name'],
                    'value' => $value,
                    'items' => array_map(
                        function ($key, $levelName) use ($name, $item, $value) {
                            return isset($item[$key])
                                ? $this->bsRadio($name, $key)
                                       ->set($key == $value)
                                       ->label($levelName)
                                : '';
                        },
                        array_keys(Settings::$permissionLevels),
                        Settings::$permissionLevels
                    )
                ];
            },
            array_keys(Settings::$permissions),
            Settings::$permissions
        );

        $this->render('files/View/Admin/Permissions/Form.inc', [
            'header' => 'Správa oprávnění',
            'subheader' => (
                ($this->action == 'add')
                ? 'Přidat uživatelskou skupinu'
                : 'Upravit uživatelskou skupinu'
            ),
            'action' => $request->getAction(),
            'name' => $request->post('name') ?: ($data ? $data['pe_name'] : ''),
            'description' => $request->post('description') ?: ($data ? $data['pe_description'] : ''),
            'settings' => $settings
        ]);
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
