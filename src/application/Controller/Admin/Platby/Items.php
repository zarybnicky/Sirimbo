<?php
namespace TKOlomouc\Controller\Admin\Platby;

use TKOlomouc\Controller\Admin\Platby;
use TKOlomouc\Utility\Permissions;
use TKOlomouc\Utility\Form;
use TKOlomouc\Utility\User;
use TKOlomouc\Utility\Request;
use TKOlomouc\Type\Date;
use TKOlomouc\Model\DBUser;
use TKOlomouc\Model\DBPlatbyItem;
use TKOlomouc\Model\DBPlatbyRaw;

class Items extends Platby
{
    function __construct()
    {
        Permissions::checkError('platby', P_OWNED);
    }
    function view($id = null)
    {
        switch(post('action')) {
            case 'edit':
                $users = post('data');
                if ($users[0]) {
                    $this->redirect('/admin/platby/items/edit/' . $users[0]);
                }
                break;
            case 'remove':
                if (is_array(post('data'))) {
                    $this->redirect(
                        '/admin/platby/items/remove?'
                        . http_build_query(array('u' => post('data')))
                    );
                }
                break;
        }
        $data = $this->_getData();
        $this->render(
            'src/application/View/Admin/Platby/ItemsOverview.inc',
            array(
                'users' => DBUser::getUsers(),
                'categories' => $this->_getCategories(),
                'data' => $data
            )
        );
    }
    function add($id = null)
    {
        if (empty($_POST)) {
            $this->_displayForm(0);
            return;
        } elseif (!is_object($item = $this->getFromPost())) {
            $this->redirect()->setMessage($item);
            $this->_displayForm(0);
            return;
        }
        DBPlatbyItem::insert(
            $item->variable, $item->category_id, null, $item->amount,
            $item->date, $item->prefix
        );
        $this->redirect('/admin/platby/items', 'Platba úspěšně přidána');
    }
    function edit($id = null)
    {
        if (!$id || !($data = DBPlatbyItem::getSingle($id))) {
            $this->redirect('/admin/platby/items', 'Platba s takovým ID neexistuje');
        }
        if (empty($_POST)) {
            post('date', $data['pi_date']);
            post('amount', $data['pi_amount']);
            post('variable', $data['pi_id_user']);
            post('specific', $data['pi_id_category']);
            post('prefix', $data['pi_prefix']);
            $this->_displayForm($id);
            return;
        } elseif (!is_object($item = $this->getFromPost($id))) {
            $this->redirect()->setMessage($item);
            $this->_displayForm($id);
            return;
        }
        DBPlatbyItem::update(
            $id, $item->variable, $item->category_id, $item->amount,
            $item->date, $item->prefix
        );
        $this->redirect('/admin/platby/items', 'Platba úspěšně upravena');
    }
    function remove($id = null)
    {
        if (!is_array(post('data')) && !is_array(get('u'))) {
            $this->redirect('/admin/platby/items');
        }
        if (!empty($_POST) && post('action') == 'confirm') {
            foreach (post('data') as $id) {
                $item = DBPlatbyItem::getSingle($id);
                $itemRaw = DBPlatbyRaw::getSingle($item['pi_id_raw']);

                DBPlatbyItem::remove($id);
                if ($item['pi_id_raw']) {
                    DBPlatbyRaw::update(
                        $item['pi_id_raw'], $itemRaw['pr_raw'],
                        $itemRaw['pr_hash'], '0', '1');
                }
            }
            $this->redirect('/admin/platby/items', 'Platby odebrány');
        }
        $data = array();
        foreach (get('u') as $id) {
            $item = DBPlatbyItem::getSingle($id, true);
            $data[] = array(
                'id' => $item['pi_id'],
                'text' => $item['u_jmeno'] . ' ' . $item['u_prijmeni']
                    . ' - ' . $item['pc_name']
            );
        }
        $this->render(
            'src/application/View/Admin/RemovePrompt.inc',
            array(
                'header' => 'Správa plateb',
                'prompt' => 'Opravdu chcete odstranit platby:',
                'returnURL' => Request::getReferer(),
                'data' => $data
            )
        );
    }
    private function _displayForm($id)
    {
        $raw = array();
        if ($id && ($item = DBPlatbyItem::getSingle($id))
            && ($data = DBPlatbyRaw::getSingle($item['pi_id_raw']))
        ) {
            $data = unserialize($data['pr_raw']);
            foreach ($data as $key => $value) {
                $raw[] = array(
                    'column' => $key,
                    'value' => $value
                );
            }
        }
        $users = $this->_getUsers();
        $categories = $this->_getCategories();
        $this->render(
            'src/application/View/Admin/Platby/ItemsForm.inc',
            array(
                'action' => Request::getAction(),
                'id' => $id,
                'raw' => $raw,
                'users' => $users,
                'categories' => $categories,
                'referer' => Request::getReferer()
            )
        );
    }
    private function _getCategories()
    {
        $out = $this->getCategoryLookup(false, false, true);
        foreach ($out as $key => &$array) {
            if (strpos($key, 'group_') !== false) {
                $array = "{$array['pg_name']}:";
            } else {
                $array = "{$array['pc_symbol']} - {$array['pc_name']}";
            }
        }
        return $out;
    }
    private function _getUsers()
    {
        $users = $this->getUserLookup(true);
        foreach ($users as $key => &$array) {
            $array = User::varSymbol($array['u_id']) . " - {$array['u_prijmeni']}, {$array['u_jmeno']}";
        }
        return $users;
    }
    private function _getData()
    {
        $filter = array();
        if (get('user') && is_numeric(get('user'))) {
            $filter['u_id'] = get('user');
        }
        $data = DBPlatbyItem::get(true, $filter);
        foreach ($data as &$row) {
            $new_data = array(
                'checkBox' => '<input type="checkbox" name="data[]" value="' . $row['pi_id'] . '" />',
                'fullName' => $row['u_prijmeni'] . ', ' . $row['u_jmeno'],
                'category' => $row['pc_name'],
                'date' => (new Date($row['pi_date']))->getDate(Date::FORMAT_SIMPLE_SPACED),
                'amount' => $row['pi_amount'] . 'Kč'
            );
            $row = $new_data;
        }
        return $data;
    }
}