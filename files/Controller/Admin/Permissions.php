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
                    http_build_query(array('u' => $request->post('permissions')))
                );
                break;
        }
        $data = array_map(
            function ($item) {
                return array(
                    'checkBox' => $this->checkbox('permissions[]', $item['pe_id']),
                    'name' => $item['pe_name'],
                    'description' => $item['pe_description']
                );
            },
            DBPermissions::getGroups()
        );
        $this->render(
            'files/View/Admin/Permissions/Overview.inc',
            array(
                'showMenu' => !TISK,
                'data' => $data
            )
        );
    }

    public function add($request)
    {
        if (!$request->post() || is_object($f = $this->checkData($request))) {
            if ($request->post) {
                $this->redirect()->setMessage($f->getMessages());
            }
            $this->render(
                'files/View/Admin/Permissions/Form.inc',
                array(
                    'action' => $request->getAction(),
                    'name' => $request->post('name'),
                    'description' => $request->post('description')
                )
            );
            return;
        }
        $permissions = array();
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

        if (!$request->post() || is_object($f = $this->checkData($request))) {
            if (!$request->post()) {
                $request->post('name', $data['pe_name']);
                $request->post('description', $data['pe_description']);
                foreach (Settings::$permissions as $name => $item) {
                    $request->post($name, $data['pe_' . $name]);
                }
            } else {
                $this->redirect()->setMessage($f->getMessages());
            }
            $this->render(
                'files/View/Admin/Permissions/Form.inc',
                array(
                    'action' => $request->getAction(),
                    'name' => $request->post('name'),
                    'description' => $request->post('description')
                )
            );
            return;
        }
        $permissions = array();
        foreach (Settings::$permissions as $name => $item) {
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
        $data = array();
        foreach ($request->get('u') as $id) {
            $item = DBPermissions::getSingleGroup($id);
            $data[] = array(
                'id' => $item['pe_id'],
                'text' => $item['pe_name']
            );
        }
        $this->render(
            'files/View/Admin/RemovePrompt.inc',
            array(
                'header' => 'Správa oprávnění',
                'prompt' =>
                    $this->notice('Bude nutné přiřadit uživatelům z těchto skupin jinou skupinu!')
                    . 'Opravdu chcete odstranit uživatelské úrovně:',
                'returnURI' => $request->getReferer(),
                'data' => $data
            )
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
