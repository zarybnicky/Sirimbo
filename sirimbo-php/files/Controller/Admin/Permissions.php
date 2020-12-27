<?php
namespace Olymp\Controller\Admin;

class Permissions
{
    public static function list()
    {
        \Permissions::checkError('permissions', P_ADMIN);
        $data = array_map(
            fn($item) => [
                'buttons' => \Buttons::permission($item['pe_id']),
                'name' => $item['pe_name'],
                'description' => $item['pe_description']
            ],
            \DBPermissions::getGroups()
        );
        \Render::page('files/View/Admin/Permissions/Overview.inc', [
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
            \Message::warning($form->getMessages());
            return static::renderForm('add');
        }
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
        $form = static::checkData();
        if (!$form->isValid()) {
            \Message::warning($form->getMessages());
            return static::renderForm('edit', $data);
        }
        $permissions = [];
        foreach (array_keys(\Permissions::$permissions) as $name) {
            $permissions[$name] = $_POST[$name];
        }
        \DBPermissions::editGroup($id, $_POST['name'], $_POST['description'], $permissions);
        \Redirect::to($_POST['returnURI'] ?? '/admin/permissions');
    }

    public static function remove($id)
    {
        \Permissions::checkError('permissions', P_ADMIN);
        $item = \DBPermissions::getSingleGroup($id);
        \Render::twig('RemovePrompt.twig', [
            'header' => 'Správa oprávnění',
            'prompt' =>
                \Utils::notice('Uživatelům z této skupiny bude nutné přiřadit jinou skupinu!')
                . 'Opravdu chcete odstranit uživatelskou úroveň:',
            'returnURI' => $_SERVER['HTTP_REFERER'] ?? '/admin/permissions',
            'data' => [['id' => $item['pe_id'], 'text' => $item['pe_name']]]
        ]);
    }

    public static function removePost($id)
    {
        \Permissions::checkError('permissions', P_ADMIN);
        \DBPermissions::removeGroup($id);
        \Message::info('Úroveň odebrána. Nezapomeňte přiřadit uživatelům z této skupiny jinou skupinu!');
        \Redirect::to('/admin/permissions');
    }

    protected static function renderForm($action, $data = [])
    {
        if (!$_POST) {
            foreach (\Permissions::$permissions as $name => $item) {
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
                        ? \Utils::radio($name, $key, $key == $value, $levelName)
                        : '',
                        [P_NONE, P_VIEW, P_MEMBER, P_OWNED, P_ADMIN],
                        ['Bez přístupu', 'Zobrazit', 'Editovat', 'Admin (svoje)', 'Admin']
                    )
                ];
            },
            array_keys(\Permissions::$permissions),
            \Permissions::$permissions
        );

        $action = !$data ? 'Přidat' : 'Upravit';
        \Render::page('files/View/Admin/Permissions/Form.inc', [
            'header' => 'Správa oprávnění',
            'subheader' => "$action uživatelskou skupinu",
            'action' => $action,
            'name' => $_POST['name'] ?? $data['pe_name'] ?? '',
            'description' => $_POST['description'] ?? $data['pe_description'] ?? '',
            'settings' => $settings
        ]);
    }

    private static function checkData(): \Form
    {
        $f = new \Form();
        foreach (\Permissions::$permissions as $name => $item) {
            $f->checkArrayKey($_POST[$name], $item, "Neplatná hodnota práva $name", $name);
        }
        return $f;
    }
}
