<?php
require_once 'files/Controller/Admin.php';
class Controller_Admin_Skupiny extends Controller_Admin
{
    function __construct() {
        Permissions::checkError('skupiny', P_OWNED);
    }
    function view($id = null) {
        switch(post('action')) {
            case 'edit':
                $skupiny = post('data');
                if ($skupiny[0])
                    $this->redirect('/admin/skupiny/edit/' . $skupiny[0]);
                break;
            case 'remove':
                if (!is_array(post('data')))
                    break;
                $this->redirect('/admin/skupiny/remove?' . http_build_query(array('u' => post('data'))));
                break;
        }
        $data = DBSkupiny::get();
        foreach ($data as $key => &$item) {
            $new_data = array(
                'buttons' => $this->_getEditLink('/admin/skupiny/edit/' . $item['s_id']) .
                    $this->_getRemoveLink('/admin/skupiny/remove/' . $item['s_id']),
                'colorBox' => $this->colorbox($item['s_color_rgb'], $item['s_description']),
                'name' => $item['s_name']
            );
            $item = $new_data;
        }
        $this->render(
            'files/View/Admin/Skupiny/Overview.inc',
            array(
                'showMenu' => !TISK,
                'data' => $data
            )
        );
    }
    function add($id = null) {
        if (empty($_POST) || is_object($f = $this->_checkPost())) {
            if (!empty($_POST)) {
                $this->redirect()->setMessage($f->getMessages());
            }
            $this->_displayForm('add');
            return;
        }
        DBSkupiny::insert(post('name'), post('color'), post('desc'));
        $insertId = DBSkupiny::getInsertId();
        if (get('group') && ($data = DBPlatbyGroup::getSingle(get('group')))) {
            DBSkupiny::addChild($insertId, get('group'));
            $conflicts = DBPlatby::checkConflicts($insertId);
            if (!empty($conflicts)) {
                DBSkupiny::removeChild($insertId, get('group'));
                $this->redirect(
                    '/admin/platby/structure/group/edit/' . get('group'),
                    'Skupina byla přidána, ale nebyla přiřazena - takové přiřazení není platné.'
                );
            }
            $this->redirect(
                '/admin/platby/structure/group/edit/' . get('group'),
                'Skupina úspěšně přidána a přiřazena'
            );
        }
        $this->redirect('/admin/skupiny', 'Skupina úspěšně přidána');
    }
    function edit($id = null) {
        if (!$id || !($data = DBSkupiny::getSingle($id)))
            $this->redirect('/admin/skupiny', 'Skupina s takovým ID neexistuje');

        if (post('action') == 'group') {
            if (!($data = DBPlatbyGroup::getSingle(post('group'))))
                $this->redirect('/admin/skupiny/edit/' . $id, 'Kategorie s takovým ID neexistuje.');

            DBSkupiny::addChild($id, post('group'));
            $conflicts = DBPlatby::checkConflicts($id);

            if (!empty($conflicts)) {
                DBSkupiny::removeChild($id, post('group'));
                $this->redirect(
                    '/admin/skupiny/edit/' . $id,
                    'Takové přiřazení není platné - způsobilo by, že jeden specifický symbol by byl v jedné skupině dvakrát.'
                );
            }
            $this->redirect('/admin/skupiny/edit/' . $id, 'Kategorie byla úspěšně přiřazena.');
        } elseif (post('action') == 'group_remove') {
            if (!($data = DBPlatbyGroup::getSingle(post('group'))))
                $this->redirect('/admin/skupiny/edit/' . $id, 'Kategorie s takovým ID neexistuje.');

            DBSkupiny::removeChild($id, post('group'));
            $this->redirect('/admin/skupiny/edit/' . $id, 'Spojení s kategorií bylo úspěšně odstraněno.');
        }

        if (empty($_POST) || is_object($f = $this->_checkPost())) {
            if (empty($_POST)) {
                post('name', $data['s_name']);
                post('color', $data['s_color_rgb']);
                post('popis', $data['s_description']);
            } else {
                $this->redirect()->setMessage($f->getMessages());
            }
            $this->_displayForm('edit');
            return;
        }
        DBSkupiny::update($id, post('name'), post('color'), post('desc'));
        $this->redirect('/admin/skupiny', 'Skupina úspěšně upravena');
    }
    function remove($id = null) {
        if (!$id || !($data = DBSkupiny::getSingle($id)))
            $this->redirect('/admin/skupiny', 'Skupina s takovým ID neexistuje');

        if (post('action') == 'unlink') {
            $f = $this->_getLinkedSkupinaObjects($id);

            $groupCount = 0;
            foreach ($f['groups'] as $data) {
                DBSkupiny::removeChild($id, $data['pg_id']);
                ++$groupCount;
            }
            unset($data);
            $this->redirect(
                '/admin/platby/structure/category/remove/' . $id,
                'Spojení s ' . $groupCount . ' kategoriemi byla odstraněna.'
            );
            return;
        }
        if (((empty($_POST) || post('action') == 'confirm') && ($f = $this->_getLinkedSkupinaObjects($id))) || empty($_POST)) {
            if (isset($f) && $f) {
                $this->redirect()->setMessage(
                    'Nemůžu odstranit skupinu s připojenými kategoriemi! '
                    . '<form action="" method="post">'
                    . '<button type="submit" name="action" value="unlink">'
                    . 'Odstranit spojení?</button>'
                    . '</form>'
                );
            }
            $this->render(
                'files/View/Admin/RemovePrompt.inc',
                array(
                    'header' => 'Správa skupin',
                    'prompt' => 'Opravdu chcete odstranit skupinu?',
                    'returnURI' => Request::getReferer(),
                    'data' => array(array('id' => $data['s_id'], 'text' => $data['s_name']))
                )
            );
            return;
        }
        DBSkupiny::delete($id);
        $this->redirect('/admin/skupiny', 'Skupina byla úspěšně odebrána.');
    }
    private function _displayForm($action) {
        $id = Request::getID() ? Request::getID() : '0';

        $groups = DBSkupiny::getSingleWithGroups($id);
        foreach ($groups as &$array) {
            $new_data = array(
                'buttons' => '<form action="" method="post">'
                . $this->_getUnlinkGroupButton($array['pg_id'])
                . $this->_getEditLink('/admin/platby/structure/group/edit/' . $array['pg_id'])
                . $this->_getRemoveLink('/admin/platby/structure/group/remove/' . $array['pg_id'])
                . '</form>',
                'type' => ($array['pg_type'] == '1' ? 'Členské příspěvky' : 'Běžné platby'),
                'name' => $array['pg_name'],
                'base' => $array['pg_base']
            );
            $array = $new_data;
        } unset($array);

        $groupNotInSkupina = DBPlatbyGroup::getNotInSkupina($id);
        $groupSelect = array();
        foreach ($groupNotInSkupina as $array) {
            $groupSelect[$array['pg_id']] = $array['pg_name'];
        } unset($array);

        $this->render(
            'files/View/Admin/Skupiny/Form.inc',
            array(
                'id' => $id,
                'action' => $action,
                'groups' => $groups,
                'groupSelect' => $groupSelect
            )
        );
    }
    private function _getLinkedSkupinaObjects($id) {
        $group = DBSkupiny::getSingleWithGroups($id);

        if (empty($group))
            return array();
        else
            return array('groups' => $group);
    }
    private function _checkPost() {
        $f = new Form();

        $f->checkNotEmpty(post('name'), 'Zadejte prosím nějaké jméno.');
        $f->checkNotEmpty(post('desc'), 'Zadejte prosím nějaký popis.');
        $f->checkRegexp(post('color'), '/#[0-9a-f]{6}/i', 'Zadejte prosím platnou barvu.');

        return $f->isValid() ? true : $f;
    }
    private function _getEditLink($link) {
        return '<a href="' . $link . '"><img alt="Upravit" src="/images/wrench.png" /></a>';
    }
    private function _getRemoveLink($link) {
        return '<a href="' . $link . '"><img alt="Odstranit" src="/images/cross.png" /></a>';
    }
    private function _getUnlinkGroupButton($id) {
        return
            '<input type="hidden" name="group" value="' . $id . '">'
            . '<button name="action" value="group_remove">'
            . '<img alt="Odstranit spojení" src="/images/unlink.png" />'
            . '</button>';
    }
}