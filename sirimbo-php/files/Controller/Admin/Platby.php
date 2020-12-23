<?php
namespace Olymp\Controller\Admin;

class Platby
{
    public static function overview()
    {
        \Permissions::checkError('platby', P_OWNED);
        $data = \DBUser::getUsersWithSkupinaPlatby();
        $skupiny = [];
        $index = 0;
        $currentID = -1;
        $currentKey = 0;
        foreach ($data as $item) {
            if ($item['s_id'] != $currentID) {
                $index = 0;
                $currentID = $item['s_id'];
                $currentKey = count($skupiny);
                $skupiny[$currentKey] = ['users' => []];
                $skupiny[$currentKey]['info'] = [
                    'header' => "<big>"
                    . \Utils::colorbox($item['s_color_rgb'], $item['s_description'])
                    . '&nbsp;&nbsp;' . $item['s_name'] . "</big>"
                ];
            }
            $skupiny[$currentKey]['users'][] = [
                'index' => ++$index . '.',
                'fullName' => \Utils::person($item),
                'hasPaid' => new \Tag(
                    'span',
                    ['style' => 'font-weight:bold;color:' . ($item['pi_id'] ? 'green' : 'red')],
                    $item['pi_id'] ? 'ANO' : 'NE'
                ),
                'amount' => $item['pi_amount'] == ($item['pc_amount'] * $item['pg_base'])
                ? ('<span style="color:green">' . (int) $item['pi_amount'] . ' Kč</span>')
                : ('<span style="font-weight:bold;color:red">'
                   . (int) $item['pi_amount'] . ' Kč</span> ('
                   . (int) ($item['pc_amount'] * $item['pg_base']) . ' Kč)')
            ];
        }

        $columns = [[], []];
        $leftCount = 0;
        $rightCount = 0;
        foreach ($skupiny as &$skupina) {
            $skupina['info']['count'] = count($skupina['users']);
            if ($rightCount >= $leftCount) {
                $columns[0][] = $skupina;
                $leftCount += ($skupina['info']['count']);
            } else {
                $columns[1][] = $skupina;
                $rightCount += ($skupina['info']['count']);
            }
        }
        \Render::page('files/View/Admin/Platby/Statistics.inc', [
            'header' => 'Správa plateb',
            'subheader' => 'Členové podle skupin',
            'columns' => $columns,
        ]);
    }

    public static function structure()
    {
        \Permissions::checkError('platby', P_OWNED);
        \Render::page('files/View/Admin/Platby/StructureOverview.inc', [
            'header' => 'Správa plateb',
            'subheader' => 'Struktura plateb',
            'data' => array_map(
                function ($item) {
                    if (strpos($item[0], 'group_') !== false) {
                        return [
                            'name' => new \Tag(
                                'span',
                                ['class' => 'big', 'style' => 'text-decoration:underline'],
                                $item[1]['pg_name']
                            ),
                            'buttons' => \Buttons::platbyGroup($item[1]['pg_id']),
                        ];
                    } else {
                        return [
                            'name' => '&nbsp;- ' . $item[1]['pc_name'] . ' (' . $item[1]['pc_symbol'] . ')',
                            'buttons' => \Buttons::platbyCategory($item[1]['pc_id']),
                        ];
                    }
                },
                static::getCategoryList()
            ),
            'orphanGroupSkupina' => array_map(
                fn($item) => [
                    'name' => $item['pg_name'],
                    'buttons' => \Buttons::platbyGroup($item['pg_id']),
                ],
                \DBPlatbyGroup::getWithoutSkupina()
            ),
            'orphanGroupCategory' => array_map(
                fn($item) => [
                    'name' => $item['pg_name'],
                    'buttons' => \Buttons::platbyGroup($item['pg_id']),
                ],
                \DBPlatbyGroup::getWithoutCategory()
            ),
            'orphanCategory' => array_map(
                fn($item) => [
                    'name' => $item['pc_name'],
                    'buttons' => \Buttons::platbyCategory($item['pc_id']),
                ],
                \DBPlatbyCategory::getOrphan()
            ),
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

    public static function getCategoryList()
    {
        $in = \DBPlatbyGroup::getGroupsWithCategories();
        $out = [];
        $group_id = 0;
        foreach ($in as $array) {
            if ($group_id != $array['pg_id'] && !isset($out['group_' . $array['pg_id']])) {
                $out[] = ['group_' . $array['pg_id'], $array];
                $group_id = $array['pg_id'];
            }
            $out[] = [$array['pc_id'], $array];
        }
        return $out;
    }

    public static function getCategoryLookup($useSymbolKey, $unique, $includeGroups)
    {
        $in = \DBPlatbyGroup::getGroupsWithCategories();
        $out = [];
        $group_id = 0;
        foreach ($in as $array) {
            if ($includeGroups && $group_id != $array['pg_id'] && !isset($out['group_' . $array['pg_id']])) {
                $out['group_' . $array['pg_id']] = $array;
                $group_id = $array['pg_id'];
            }
            $key = (int) ($useSymbolKey ? $array['pc_symbol'] : $array['pc_id']);
            if ($unique && isset($out[$key])) {
                continue;
            }
            $out[$key] = $array;
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
        $item = new \PlatbyItem();
        $item->init(
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
