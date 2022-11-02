<?php
namespace Olymp\Controller\Admin;

class Permissions
{
    public static function add()
    {
        \Permissions::checkError('permissions', P_ADMIN);
        static::renderForm('add');
    }

    public static function addPost()
    {
        \Permissions::checkError('permissions', P_ADMIN);
        $permissions = [];
        foreach (array_keys(\Permissions::$permissions) as $name) {
            $permissions[$name] = $_POST[$name];
        }
        \DBPermissions::addGroup($_POST['name'], $_POST['description'], $permissions);
        \Redirect::to($_POST['returnURI'] ?? '/admin/permissions');
    }

    public static function edit($id)
    {
        \Permissions::checkError('permissions', P_ADMIN);
        if (!$data = \DBPermissions::getSingleGroup($id)) {
            \Message::warning('Skupina s takovým ID neexistuje');
            \Redirect::to($_POST['returnURI'] ?? '/admin/permissions');
        }
        return static::renderForm('edit', $data);
    }

    public static function editPost($id)
    {
        \Permissions::checkError('permissions', P_ADMIN);
        if (!$data = \DBPermissions::getSingleGroup($id)) {
            \Message::warning('Skupina s takovým ID neexistuje');
            \Redirect::to($_POST['returnURI'] ?? '/admin/permissions');
        }
        $permissions = [];
        foreach (array_keys(\Permissions::$permissions) as $name) {
            $permissions[$name] = $_POST[$name];
        }
        \DBPermissions::editGroup($id, $_POST['name'], $_POST['description'], $permissions);
        \Redirect::to($_POST['returnURI'] ?? '/admin/permissions');
    }

    protected static function renderForm($action, $data = [])
    {
        \Render::twig('Admin/PermissionsForm.twig', [
            'action' => $data ? 'edit' : 'add',
            'name' => $_POST['name'] ?? $data['pe_name'] ?? '',
            'description' => $_POST['description'] ?? $data['pe_description'] ?? '',
            'permissions' => \Permissions::$permissions,
            'data' => array_merge(...array_map(
                fn($name, $item) => [$name => $_POST[$name] ?? ($data ? $data['pe_' . $name] : $item['default'])],
                array_keys(\Permissions::$permissions),
                \Permissions::$permissions,
            )),
            'options' => [
                P_NONE => 'Bez přístupu',
                P_VIEW => 'Zobrazit',
                P_MEMBER => 'Editovat',
                P_OWNED => 'Admin (svoje)',
                P_ADMIN => 'Admin'
            ],
        ]);
    }
}
