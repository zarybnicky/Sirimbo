<?php
namespace Olymp\Controller\Admin;

class PlatbyManual
{
    public static function query()
    {
        \Permissions::checkError('platby', P_OWNED);
        $remaining = \DBPlatbyRaw::getUnsorted();
        $remainingCount = count($remaining);
        if ($remainingCount == 0) {
            \Message::info('Nezbývají už žádné nezatříděné platby');
            \Redirect::to('/admin/platby');
        }
        \Redirect::to('/admin/platby/discarded/' . $remaining[0]['pr_id']);
    }

    public static function get($id)
    {
        \Permissions::checkError('platby', P_OWNED);
        $data = \DBPlatbyRaw::getSingle($id);
        $raw = unserialize($data['pr_raw']);
        if ($data['pr_sorted']) {
            \Message::info('Platba už byla zařazena do systému');
            \Redirect::to('/admin/platby/discarded');
        }

        $categoryLookup = Platby::getCategoryLookup(true, true, false);
        $userLookup = Platby::getUserLookup(false);

        $item = new \PlatbyItem();
        Platby::recognizeHeaders($raw, $specific, $variable, $date, $amount);
        $raw[null] = null;
        $item->init($raw[$specific], $raw[$variable], $raw[$date], $raw[$amount]);
        $item->processWithSymbolLookup($userLookup, $categoryLookup);

        $emptyItem = ['column' => '&nbsp;---', 'value' => '&nbsp;---'];

        if ($variable === null) {
            $recognized['variable'] = $emptyItem;
        } elseif ($item->variable) {
            $recognized['variable'] = [
                'column' => $variable,
                'value' => $userLookup[$item->variable]['u_jmeno'] . ' '
                    . $userLookup[$item->variable]['u_prijmeni']
            ];
        } else {
            $recognized['variable'] = [
                'column' => $variable,
                'value' => '&nbsp;--- (není v DB: id ' . $raw[$variable] . ')'
            ];
        }

        if ($specific === null) {
            $recognized['specific'] = $emptyItem;
        } elseif ($item->specific && $item->categoryId) {
            $recognized['specific'] = [
                'column' => $specific,
                'value' => $item->specific
            ];
        } else {
            $recognized['specific'] = [
                'column' => $specific,
                'value' => '&nbsp;--- (není v DB: symbol ' . $raw[$specific] . ')'
            ];
        }

        if ($date === null) {
            $recognized['date'] = $emptyItem;
        } else {
            $recognized['date'] = [
                'column' => $date,
                'value' => (new \Date($item->date))->getHumanDate()
            ];
        }

        if ($amount === null) {
            $recognized['amount'] = $emptyItem;
        } else {
            $recognized['amount'] = ['column' => $amount, 'value' => $raw[$amount]];
        }

        $recognized['prefix'] = [
            'column' => '&nbsp;---',
            'value' => ($item->prefix ? $item->prefix : '&nbsp;---')
        ];

        $new = [];
        foreach ($raw as $key => &$value) {
            $new[] = ['column' => $key, 'value' => $value];
        }
        $raw = $new;

        $remainingCount = count(\DBPlatbyRaw::getUnsorted());
        new \RenderHelper('files/View/Admin/Platby/ManualForm.inc', [
            'header' => 'Správa plateb',
            'subheader' => 'Ruční třídění plateb</span> (zbývá ' . $remainingCount . ')',
            'id' => $id,
            'remainingTotal' => $remainingCount,
            'raw' => $raw,
            'guess' => [
                'specific' => $item->categoryId,
                'variable' => $item->variable,
                'date' => (new \Date($item->date))->getHumanDate(),
                'amount' => $item->amount,
                'prefix' => $item->prefix
            ],
            'users' => static::getUsers(),
            'categories' => static::getCategories(),
            'recognized' => $recognized,
        ]);
    }

    public static function post($id)
    {
        \Permissions::checkError('platby', P_OWNED);
        if (!($data = \DBPlatbyRaw::getSingle($id))) {
            \Message::warning('Zadaná platba neexistuje.');
            return;
        }
        if ($data['pr_sorted']) {
            \Message::info('Zadaná platba už byla zařazená.');
            return;
        }
        switch ($_POST['action']) {
            case 'confirm':
                if (!is_object($item = Platby::getFromPost())) {
                    \Message::warning($item);
                    return;
                }
                \DBPlatbyRaw::update($id, $data['pr_raw'], $data['pr_hash'], '1', '0');
                \DBPlatbyItem::insert(
                    $item->variable,
                    $item->categoryId,
                    $id,
                    $item->amount,
                    $item->date,
                    $item->prefix
                );
                break;
            case 'discard':
                \DBPlatbyRaw::update($id, $data['pr_raw'], $data['pr_hash'], '0', '1');
                break;
            case 'skip':
                \DBPlatbyRaw::skip($id);
                break;
            default:
                \Message::danger('Neplatná POST akce.');
                break;
        }
        \Redirect::to('/admin/platby/manual');
    }

    private static function getCategories()
    {
        $categories = Platby::getCategoryLookup(false, false, true);
        $res = [];
        foreach ($categories as $key => $array) {
            if (strpos($key, 'group_') === false) {
                $res[$key] = "{$array['pc_symbol']} - {$array['pc_name']}";
            }
        }
        return $res;
    }

    private static function getUsers()
    {
        return array_map(
            fn($array) => \User::varSymbol($array['u_id']) . " - {$array['u_prijmeni']}, {$array['u_jmeno']}",
            Platby::getUserLookup(true),
        );
    }
}
