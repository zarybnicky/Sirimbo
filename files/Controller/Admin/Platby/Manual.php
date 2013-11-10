<?php
require_once 'files/Controller/Admin/Platby.php';
class Controller_Admin_Platby_Manual extends Controller_Admin_Platby
{
    function __construct()
    {
        Permissions::checkError('platby', P_OWNED);
    }
    function view($id = null)
    {
        if (!empty($_POST)) {
            $this->_processPost();
        }
        $remaining = DBPlatbyRaw::getUnsorted();
        $remainingCount = count($remaining);

        if ($id && ($data = DBPlatbyRaw::getSingle($id))) {
            if ($data['pr_sorted']) {
                $this->redirect(
                    '/admin/platby/discarded',
                    'Platba už byla zařazena do systému'
                );
            }
            $raw = unserialize($data['pr_raw']);
        } else {
            if ($remainingCount == 0) {
                $this->redirect(
                    '/admin/platby',
                    'Nezbývají už žádné nezatříděné platby'
                );
            }
            $id = $remaining[0]['pr_id'];
            $raw = unserialize($remaining[0]['pr_raw']);
        }

        $categoryLookup = $this->getCategoryLookup(true, true, false);
        $userLookup = $this->getUserLookup(false);

        $item = new PlatbyItem();
        $this->recognizeHeaders($raw, $specific, $variable, $date, $amount);
        $raw[null] = null;
        $item->init($raw[$specific], $raw[$variable], $raw[$date], $raw[$amount]);
        $item->processWithSymbolLookup($userLookup, $categoryLookup);

        $emptyItem = array(
            'column' => '&nbsp;---',
            'value' => '&nbsp;---'
        );

        if ($variable === null) {
            $recognized['variable'] = $emptyItem;
        } elseif ($item->variable) {
            $recognized['variable'] = array(
                'column' => $variable,
                'value' => $userLookup[$item->variable]['u_jmeno'] . ' '
                    . $userLookup[$item->variable]['u_prijmeni']
            );
        } else {
            $recognized['variable'] = array(
                'column' => $variable,
                'value' => '&nbsp;--- (není v DB: id ' . $raw[$variable] . ')'
            );
        }

        if ($specific === null) {
            $recognized['specific'] = $emptyItem;
        } elseif ($item->specific && $item->categoryId) {
            $recognized['specific'] = array(
                'column' => $specific,
                'value' => $item->specific
            );
        } else {
            $recognized['specific'] = array(
                'column' => $specific,
                'value' => '&nbsp;--- (není v DB: symbol ' . $raw[$specific] . ')'
            );
        }

        if ($date === null) {
            $recognized['date'] = $emptyItem;
        } else {
            $recognized['date'] = array(
                'column' => $date,
                'value' => (new Date($item->date))->getDate(Date::FORMAT_SIMPLIFIED)
            );
        }

        if ($amount === null) {
            $recognized['amount'] = $emptyItem;
        } else {
            $recognized['amount'] = array(
                'column' => $amount,
                'value' => $raw[$amount]
            );
        }

        $recognized['prefix'] = array(
            'column' => '&nbsp;---',
            'value' => ($item->prefix ? $item->prefix : '&nbsp;---')
        );

        $new = array();
        foreach ($raw as $key => &$value) {
            $new[] = array(
                'column' => $key,
                'value' => $value
            );
        }
        $raw = $new;

        $this->render(
            'files/View/Admin/Platby/ManualForm.inc',
            array(
                'id' => $id,
                'remainingTotal' => $remainingCount,
                'raw' => $raw,
                'guess' => array(
                    'specific' => $item->categoryId,
                    'variable' => $item->variable,
                    'date' => (new Date($item->date))->getDate(Date::FORMAT_SIMPLIFIED),
                    'amount' => $item->amount,
                    'prefix' => $item->prefix
                ),
                'users' => $this->_getUsers(),
                'categories' => $this->_getCategories(),
                'recognized' => $recognized
            )
        );
    }

    private function _getCategories()
    {
        $categories = parent::getCategoryLookup(false, false, true);
        foreach ($categories as $key => &$array) {
            if (strpos($key, 'group_') !== false) {
                $array = "{$array['pg_name']}:";
            } else {
                $array = "{$array['pc_symbol']} - {$array['pc_name']}";
            }
        }
        return $categories;
    }

    private function _getUsers()
    {
        $users = parent::getUserLookup(true);
        foreach ($users as &$array) {
            $array = User::varSymbol($array['u_id']) . " - {$array['u_prijmeni']}, {$array['u_jmeno']}";
        }
        return $users;
    }

    private function _processPost()
    {
        if (!post('id') || !($current = DBPlatbyRaw::getSingle(post('id')))) {
            $this->redirect()->setMessage('Zadaná platba neexistuje.');
            return;
        } elseif ($current['pr_sorted'] && ($item = DBPlatbyItem::getSingleByRawId(post('id')))) {
            $this->redirect()->setMessage('Zadaná platba už byla zařazená.');
            return;
        }
        if (post('action') == 'confirm') {
            if (empty($_POST)) {
                return;
            } elseif (!is_object($item = $this->getFromPost())) {
                $this->redirect()->setMessage($item);
                return;
            }
            DBPlatbyRaw::update(post('id'), $current['pr_raw'], $current['pr_hash'], '1', '0');
            DBPlatbyItem::insert($item->variable, $item->categoryId, post('id'), $item->amount, $item->date, $item->prefix);
        } elseif (post('action') == 'discard') {
            if (!$current['pr_discarded'])
                DBPlatbyRaw::update(post('id'), $current['pr_raw'], $current['pr_hash'], '0', '1');
        } elseif (post('action') == 'skip') {
            DBPlatbyRaw::skip(post('id'));
        } else {
            $this->redirect()->setMessage('Neplatná POST akce.');
        }
        $this->redirect('/admin/platby/manual');
    }
}