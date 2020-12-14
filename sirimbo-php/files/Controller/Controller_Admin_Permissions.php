<?php
class Controller_Admin_Permissions
{
    public function view($request)
    {
        \Permissions::checkError('permissions', P_ADMIN);
        $data = array_map(
            function ($item) {
                return [
                    'buttons' => new EditLinkHelper('/admin/permissions/edit/' . $item['pe_id'])
                        . '&nbsp;&nbsp;'
                        . new RemoveLinkHelper('/admin/permissions/remove/' . $item['pe_id']),
                    'name' => $item['pe_name'],
                    'description' => $item['pe_description']
                ];
            },
            DB\Permissions::getGroups()
        );
        new \RenderHelper('files/View/Admin/Permissions/Overview.inc', [
            'header' => 'Správa oprávnění',
            'data' => $data
        ]);
    }

    public function add($request)
    {
        \Permissions::checkError('permissions', P_ADMIN);
        if (!$_POST) {
            return $this->renderForm($request);
        }
        $form = $this->checkData($request);
        if (!$form->isValid()) {
            new \MessageHelper('warning', $form->getMessages());
            return $this->renderForm($request);
        }

        $permissions = [];
        foreach (array_keys(Settings::$permissions) as $name) {
            $permissions[$name] = $_POST[$name];
        }
        DB\Permissions::addGroup(
            $_POST['name'],
            $_POST['description'],
            $permissions
        );

        new \RedirectHelper($_POST['returnURI'] ?: '/admin/permissions');
    }

    public function edit($request)
    {
        \Permissions::checkError('permissions', P_ADMIN);
        if (!$id = $request->getId()) {
            new \MessageHelper('warning', 'Skupina s takovým ID neexistuje');
            new \RedirectHelper($_POST['returnURI'] ?: '/admin/permissions');
        }
        if (!$data = DB\Permissions::getSingleGroup($id)) {
            new \MessageHelper('warning', 'Skupina s takovým ID neexistuje');
            new \RedirectHelper($_POST['returnURI'] ?: '/admin/permissions');
        }

        if (!$_POST) {
            return $this->renderForm($request, $data);
        }
        $form = $this->checkData($request);
        if (!$form->isValid()) {
            new \MessageHelper('warning', $form->getMessages());
            return $this->renderForm($request, $data);
        }

        $permissions = [];
        foreach (array_keys(Settings::$permissions) as $name) {
            $permissions[$name] = $_POST[$name];
        }
        DB\Permissions::editGroup(
            $id,
            $_POST['name'],
            $_POST['description'],
            $permissions
        );

        new \RedirectHelper($_POST['returnURI'] ?: '/admin/permissions');
    }

    public function remove($request)
    {
        \Permissions::checkError('permissions', P_ADMIN);
        if (!$request->getId()) {
            new \RedirectHelper('/admin/permissions');
        }
        $id = $request->getId();

        if ($_POST['action'] == 'confirm') {
            DB\Permissions::removeGroup($id);
            new \MessageHelper('info', 'Úroveň odebrána. Nezapomeňte přiřadit uživatelům z této skupiny jinou skupinu!');
            new \RedirectHelper('/admin/permissions');
        }

        $item = DB\Permissions::getSingleGroup($id);
        new \RenderHelper('files/View/Admin/RemovePrompt.inc', [
            'header' => 'Správa oprávnění',
            'prompt' =>
                new NoticeHelper('Uživatelům z této skupiny bude nutné přiřadit jinou skupinu!')
                . 'Opravdu chcete odstranit uživatelskou úroveň:',
            'returnURI' => $request->getReferer() ?: '/admin/permissions',
            'data' => [['id' => $item['pe_id'], 'text' => $item['pe_name']]]
        ]);
    }

    protected function renderForm($request, $data = null)
    {
        if (!$_POST) {
            foreach (Settings::$permissions as $name => $item) {
                $_POST[$name] = $data['pe_' . $name];
            }
        }
        $settings = array_map(
            function ($name, $item) use ($request, $data) {
                $value = $_POST[$name] ?: ($data ? $data['pe_' . $name] : $item['default']);
                return [
                    'name' => $item['name'],
                    'value' => $value,
                    'items' => array_map(
                        function ($key, $levelName) use ($name, $item, $value) {
                            return isset($item[$key])
                                ? (new BsRadioHelper($name, $key))
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

        new \RenderHelper('files/View/Admin/Permissions/Form.inc', [
            'header' => 'Správa oprávnění',
            'subheader' => (
                ($data === null)
                ? 'Přidat uživatelskou skupinu'
                : 'Upravit uživatelskou skupinu'
            ),
            'action' => $request->getAction(),
            'name' => $_POST['name'] ?: ($data ? $data['pe_name'] : ''),
            'description' => $_POST['description'] ?: ($data ? $data['pe_description'] : ''),
            'settings' => $settings
        ]);
    }

    private function checkData($request): \Form
    {
        $f = new \Form();
        foreach (Settings::$permissions as $name => $item) {
            $f->checkArrayKey(
                $_POST[$name], $item,
                'Neplatná hodnota práva "' . $name . '"', $name
            );
        }
        return $f;
    }
}
