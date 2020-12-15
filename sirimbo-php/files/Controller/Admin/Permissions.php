<?php
namespace Olymp\Controller\Admin;

class Permissions
{
    public static function list()
    {
        \Permissions::checkError('permissions', P_ADMIN);
        $data = array_map(
            fn($item) => [
                'buttons' => new \EditLinkHelper('/admin/permissions/edit/' . $item['pe_id'])
                . '&nbsp;&nbsp;'
                . new \RemoveLinkHelper('/admin/permissions/remove/' . $item['pe_id']),
                'name' => $item['pe_name'],
                'description' => $item['pe_description']
            ],
            \DBPermissions::getGroups()
        );
        new \RenderHelper('files/View/Admin/Permissions/Overview.inc', [
            'header' => 'Správa oprávnění',
            'data' => $data
        ]);
    }

    public static function add()
    {
        \Permissions::checkError('permissions', P_ADMIN);
        static::renderForm('add');
    }

    public static function addPost()
    {
        \Permissions::checkError('permissions', P_ADMIN);
        $form = static::checkData();
        if (!$form->isValid()) {
            new \MessageHelper('warning', $form->getMessages());
            return static::renderForm('add');
        }
        $permissions = [];
        foreach (array_keys(\Settings::$permissions) as $name) {
            $permissions[$name] = $_POST[$name];
        }
        \DBPermissions::addGroup($_POST['name'], $_POST['description'], $permissions);
        new \RedirectHelper($_POST['returnURI'] ?: '/admin/permissions');
    }

    public static function edit($id)
    {
        \Permissions::checkError('permissions', P_ADMIN);
        if (!$data = \DBPermissions::getSingleGroup($id)) {
            new \MessageHelper('warning', 'Skupina s takovým ID neexistuje');
            new \RedirectHelper($_POST['returnURI'] ?: '/admin/permissions');
        }
        return static::renderForm('edit', $data);
    }

    public static function editPost($id)
    {
        \Permissions::checkError('permissions', P_ADMIN);
        if (!$data = \DBPermissions::getSingleGroup($id)) {
            new \MessageHelper('warning', 'Skupina s takovým ID neexistuje');
            new \RedirectHelper($_POST['returnURI'] ?: '/admin/permissions');
        }
        $form = static::checkData();
        if (!$form->isValid()) {
            new \MessageHelper('warning', $form->getMessages());
            return static::renderForm('edit', $data);
        }
        $permissions = [];
        foreach (array_keys(\Settings::$permissions) as $name) {
            $permissions[$name] = $_POST[$name];
        }
        \DBPermissions::editGroup($id, $_POST['name'], $_POST['description'], $permissions);
        new \RedirectHelper($_POST['returnURI'] ?: '/admin/permissions');
    }

    public static function remove($id)
    {
        \Permissions::checkError('permissions', P_ADMIN);
        $item = \DBPermissions::getSingleGroup($id);
        new \RenderHelper('files/View/Admin/RemovePrompt.inc', [
            'header' => 'Správa oprávnění',
            'prompt' =>
                new \NoticeHelper('Uživatelům z této skupiny bude nutné přiřadit jinou skupinu!')
                . 'Opravdu chcete odstranit uživatelskou úroveň:',
            'returnURI' => $_SERVER['HTTP_REFERER'] ?: '/admin/permissions',
            'data' => [['id' => $item['pe_id'], 'text' => $item['pe_name']]]
        ]);
    }

    public static function removePost($id)
    {
        \Permissions::checkError('permissions', P_ADMIN);
        \DBPermissions::removeGroup($id);
        new \MessageHelper('info', 'Úroveň odebrána. Nezapomeňte přiřadit uživatelům z této skupiny jinou skupinu!');
        new \RedirectHelper('/admin/permissions');
    }

    protected static function renderForm($action, $data = null)
    {
        if (!$_POST) {
            foreach (\Settings::$permissions as $name => $item) {
                $_POST[$name] = $data['pe_' . $name];
            }
        }
        $settings = array_map(
            function ($name, $item) use ($data) {
                $value = $_POST[$name] ?? ($data ? $data['pe_' . $name] : $item['default']);
                return [
                    'name' => $item['name'],
                    'value' => $value,
                    'items' => array_map(
                        fn($key, $levelName) => isset($item[$key])
                        ? (new \BsRadioHelper($name, $key))->set($key == $value)->label($levelName)
                        : '',
                        array_keys(\Settings::$permissionLevels),
                        \Settings::$permissionLevels
                    )
                ];
            },
            array_keys(\Settings::$permissions),
            \Settings::$permissions
        );

        new \RenderHelper('files/View/Admin/Permissions/Form.inc', [
            'header' => 'Správa oprávnění',
            'subheader' => (
                ($data === null) ? 'Přidat uživatelskou skupinu' : 'Upravit uživatelskou skupinu'
            ),
            'action' => $action,
            'name' => $_POST['name'] ?: ($data ? $data['pe_name'] : ''),
            'description' => $_POST['description'] ?: ($data ? $data['pe_description'] : ''),
            'settings' => $settings
        ]);
    }

    private static function checkData(): \Form
    {
        $f = new \Form();
        foreach (\Settings::$permissions as $name => $item) {
            $f->checkArrayKey($_POST[$name], $item, "Neplatná hodnota práva $name", $name);
        }
        return $f;
    }
}
