<?php
namespace TKOlomouc\Controller\Admin;

use TKOlomouc\Controller\Admin;
use TKOlomouc\Utility\Permissions;
use TKOlomouc\Utility\Request;
use TKOlomouc\Utility\Form;
use TKOlomouc\Model\DBPermissions;
use TKOlomouc\Settings;

class Permissions extends Admin
{
    public function __construct()
    {
        Permissions::checkError('permissions', P_ADMIN);
    }

    public function view($id = null)
    {
        switch(post('action')) {
            case 'edit':
                $data = post('permissions');
                if ($data[0]) {
                    $this->redirect('/admin/permissions/edit/' . $data[0]);
                }
                break;
            case 'remove':
                if (is_array(post('permissions'))) {
                    $this->redirect(
                        '/admin/permissions/remove?' . http_build_query(array('u' => post('permissions')))
                    );
                }
                break;
        }
        $data = DBPermissions::getGroups();
        foreach ($data as &$row) {
            $new_data = array(
                'checkBox' => '<input type="checkbox" name="permissions[]" value="' . $row['pe_id'] . '" />',
                'name' => $row['pe_name'],
                'description' => $row['pe_description']
            );
            $row = $new_data;
        }
        $this->render(
            'src/application/View/Admin/Permissions/Overview.inc',
            array(
                'showMenu' => !TISK,
                'data' => $data
            )
        );
    }

    public function add($id = null)
    {
        if (empty($_POST) || is_object($form = $this->checkData())) {
            if (!empty($_POST)) {
                $this->redirect()->setMessage($form->getMessages());
            }
            $this->render(
                'src/application/View/Admin/Permissions/Form.inc',
                array(
                    'action' => Request::getAction(),
                    'permissions' => Settings::$permissions,
                    'permissionLevels' => Settings::$permissionLevels
                )
            );
            return;
        }
        $permissions = array();
        foreach (Settings::$permissions as $name => $item) {
            $permissions[$name] = post($name);
        }
        DBPermissions::addGroup(post('name'), post('description'), $permissions);

        $this->redirect(post('referer') ? post('referer') : '/admin/permissions', 'Úroveň úspěšně přidána');
    }

    public function edit($id = null)
    {
        if (!$id || !($data = DBPermissions::getSingleGroup($id))) {
            $this->redirect(
                post('referer') ? post('referer') : '/admin/permissions',
                'Skupina s takovým ID neexistuje'
            );
        }

        if (empty($_POST) || is_object($f = $this->checkData())) {
            if (empty($_POST)) {
                post('name', $data['pe_name']);
                post('description', $data['pe_description']);
                foreach (Settings::$permissions as $name => $item) {
                    post($name, $data['pe_' . $name]);
                }
            } else {
                $this->redirect()->setMessage($f->getMessages());
            }
            $this->render(
                'src/application/View/Admin/Permissions/Form.inc',
                array(
                    'action' => Request::getAction(),
                    'permissions' => Settings::$permissions
                )
            );
            return;
        }
        $permissions = array();
        foreach (Settings::$permissions as $name => $item) {
            $permissions[$name] = post($name);
        }
        DBPermissions::editGroup($id, post('name'), post('description'), $permissions);

        $this->redirect(
            post('referer') ? post('referer') : '/admin/permissions',
            'Oprávnění úspěšně upravena'
        );
    }

    public function remove($id = null)
    {
        if (!is_array(post('data')) && !is_array(get('u'))) {
            $this->redirect('/admin/permissions');
        }
        if (!empty($_POST) && post('action') == 'confirm') {
            foreach (post('data') as $id)
            DBPermissions::removeGroup($id);
            $this->redirect(
                '/admin/permissions',
                'Úrovně odebrány. Nezapomeňte přiřadit uživatelům z těchto skupin jinou skupinu!'
            );
        }
        $data = array();
        foreach (get('u') as $id) {
            $item = DBPermissions::getSingleGroup($id);
            $data[] = array(
                'id' => $item['pe_id'],
                'text' => $item['pe_name']
            );
        }
        $this->render(
            'src/application/View/Admin/RemovePrompt.inc',
            array(
                'header' => 'Správa oprávnění',
                'prompt' =>
                    notice('Bude nutné přiřadit uživatelům z těchto skupin jinou skupinu!', true)
                    . 'Opravdu chcete odstranit uživatelské úrovně:',
                'returnURL' => Request::getReferer(),
                'data' => $data
            )
        );
    }

    private function checkData()
    {
        $f = new Form();

        foreach (Settings::$permissions as $name => $item) {
            $f->checkArrayKey(
                post($name), $item,
                'Neplatná hodnota práva "' . $name . '"', $name
            );
            $permissions[$name] = post($name);
        }

        return $f->isValid() ? true : $f;
    }
}
