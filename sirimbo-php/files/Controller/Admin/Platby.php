<?php
namespace Olymp\Controller\Admin;

class Platby
{
    public static function overview()
    {
        \Permissions::checkError('platby', P_OWNED);
        $data = \DBUser::getUsersWithSkupinaPlatby();
        $skupiny = [];
        $currentID = -1;
        $currentKey = 0;
        foreach ($data as $item) {
            if ($item['s_id'] != $currentID) {
                $currentID = $item['s_id'];
                $currentKey = count($skupiny);
                $skupiny[$currentKey] = [
                    'users' => [],
                    'description' => $item['s_description'],
                    'color' => $item['s_color_rgb'],
                    'name' => $item['s_name'],
                ];
            }
            $skupiny[$currentKey]['users'][] = [
                'user' => \User::fromArray($item),
                'hasPaid' => (bool) $item['pi_id'],
                'paidAmount' => $item['pi_amount'],
                'setAmount' => $item['pc_amount'] * $item['pg_base'],
            ];
        }

        $columns = [[], []];
        $leftCount = 0;
        $rightCount = 0;
        foreach ($skupiny as &$skupina) {
            $skupina['info']['count'] = count($skupina['users']);
            if ($rightCount >= $leftCount) {
                $columns[0][] = $skupina;
                $leftCount += $skupina['info']['count'];
            } else {
                $columns[1][] = $skupina;
                $rightCount += $skupina['info']['count'];
            }
        }
        \Render::twig('Admin/PlatbyStatistics.twig', ['columns' => $columns]);
    }

    public static function structure()
    {
        \Permissions::checkError('platby', P_OWNED);
        \Render::twig('Admin/PlatbyStructure.twig', [
            'data' => \DBPlatbyGroup::getGroupsWithCategories(),
            'orphanGroupSkupina' => \DBPlatbyGroup::getWithoutSkupina(),
            'orphanGroupCategory' => \DBPlatbyGroup::getWithoutCategory(),
            'orphanCategory' => \DBPlatbyCategory::getOrphan(),
        ]);
    }

    public static function recognizeHeaders($headers, &$specific, &$variable, &$date, &$amount)
    {
        foreach (array_keys($headers) as $key) {
            if (mb_stripos((string) $key, 'specif') !== false) {
                $specific = $key;
            }
            if (mb_stripos((string) $key, 'variab') !== false) {
                $variable = $key;
            }
            if (mb_stripos((string) $key, 'datum') !== false) {
                $date = $key;
            }
            if (mb_stripos((string) $key, 'částka') !== false) {
                $amount = $key;
            }
        }
    }

    public static function checkHeaders($headers, &$specific, &$variable, &$date, &$amount)
    {
        $headers = array_flip($headers);
        return isset($headers[$specific])
            && isset($headers[$variable])
            && isset($headers[$date])
            && isset($headers[$amount]);
    }

    public static function getCategoryLookup($useSymbolKey, $unique, $includeGroups)
    {
        $out = [];
        $group_id = 0;
        foreach (\DBPlatbyGroup::getGroupsWithCategories() as $array) {
            if ($includeGroups && $group_id != $array['pg_id'] && !isset($out['group_' . $array['pg_id']])) {
                $out['group_' . $array['pg_id']] = $array;
                $group_id = $array['pg_id'];
            }
            $key = (int) ($useSymbolKey ? $array['pc_symbol'] : $array['pc_id']);
            if (!$unique || !isset($out[$key])) {
                $out[$key] = $array;
            }
        }
        return $out;
    }

    public static function getUserLookup($sort)
    {
        $in = \DBUser::getUsers();
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
        $out = [];
        foreach ($in as $array) {
            $out[(int) $array['u_id']] = $array;
        }
        return $out;
    }

    public static function getFromPost($id = null)
    {
        $item = new \PlatbyItem(
            null,
            $_POST['variable'],
            $_POST['date'],
            $_POST['amount'],
            $_POST['prefix'],
            $id,
            $_POST['specific']
        );
        $item->processWithSymbolLookup(
            static::getUserLookup(false),
            static::getCategoryLookup(true, true, false)
        );

        $error = [];
        if (!$item->variable) {
            $error[] = 'Neplatné ID uživatele';
        }
        if (!$item->categoryId) {
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
