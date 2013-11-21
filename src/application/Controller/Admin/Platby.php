<?php
namespace TKOlomouc\Controller\Admin;

use TKOlomouc\Controller\Admin;
use TKOlomouc\Utility\Permissions;
use TKOlomouc\Model\DBPlatbyGroup;
use TKOlomouc\Model\DBUser;
use TKOlomouc\Type\PlatbyItem;

class Platby extends Admin
{
    public function __construct()
    {
        Permissions::checkError('platby', P_OWNED);
    }

    public function view($id = null)
    {
        $this->redirect('/admin/platby/overview');
    }

    protected function recognizeHeaders($headers, &$specific, &$variable, &$date, &$amount)
    {
        foreach ($headers as $key => $value) {
            if (mb_stripos($key, 'specif') !== false) {
                $specific = $key;
            }
            if (mb_stripos($key, 'variab') !== false) {
                $variable = $key;
            }
            if (mb_stripos($key, 'datum') !== false) {
                $date = $key;
            }
            if (mb_stripos($key, 'částka') !== false) {
                $amount = $key;
            }
        }
    }

    protected function checkHeaders($headers, &$specific, &$variable, &$date, &$amount)
    {
        $headers = array_flip($headers);

        if (isset($headers[$specific])
            && isset($headers[$variable])
            && isset($headers[$date])
            && isset($headers[$amount])
        ) {
            return true;
        } else {
            return false;
        }
    }

    protected function getCategoryList()
    {
        $in = DBPlatbyGroup::getGroupsWithCategories();
        $out = array();
        $group_id = 0;
        foreach ($in as $array) {
            if ($group_id != $array['pg_id'] && !isset($out['group_' . $array['pg_id']])) {
                $out[] = array('group_' . $array['pg_id'], $array);
                $group_id = $array['pg_id'];
            }
            $out[] = array($array['pc_id'], $array);
        }
        return $out;
    }

    protected function getCategoryLookup($useSymbolKey, $unique, $includeGroups)
    {
        $in = DBPlatbyGroup::getGroupsWithCategories();
        $out = array();
        $group_id = 0;
        foreach ($in as $array) {
            $key = (int) ($useSymbolKey ? $array['pc_symbol'] : $array['pc_id']);

            if ($includeGroups
                && $group_id != $array['pg_id']
                && !isset($out['group_' . $array['pg_id']])
            ) {
                $out['group_' . $array['pg_id']] = $array;
                $group_id = $array['pg_id'];
            }
            if ($unique && isset($out[$key])) {
                continue;
            }
            $out[$key] = $array;
        }
        return $out;
    }

    protected function getUserLookup($sort)
    {
        $in = DBUser::getUsers();
        if ($sort) {
            usort(
                $in,
                function ($a, $b) {
                    $c = $a['u_prijmeni'];
                    $d = $b['u_prijmeni'];
                    return ($c > $d ? 1 : ($c < $d ? -1 : 0));
                }
            );
        }
        $out = array();
        foreach ($in as $array) {
            $out[(int) $array['u_id']] = $array;
        }
        return $out;
    }

    protected function getFromPost($id = null)
    {
        $item = new PlatbyItem();
        $item->init(
            null, post('variable'), post('date'), post('amount'),
            post('prefix'), $id, post('specific')
        );
        $item->processWithSymbolLookup(
            $this->getUserLookup(false),
            $this->getCategoryLookup(true, true, false)
        );

        $error = array();

        if (!$item->variable) {
            $error[] = 'Neplatné ID uživatele';
        }
        if (!$item->category_id) {
            $error[] = 'Neplatné ID kategorie';
        }
        if (!$item->date) {
            $error[] = 'Neplatné datum';
        }
        if (!$item->prefix) {
            $error[] = 'Neplatný prefix';
        }
        if ($item->amount < 0) {
            $error[] = 'Neplatná částka';
        }

        return $item->isValid ? $item : $error;
    }
}
