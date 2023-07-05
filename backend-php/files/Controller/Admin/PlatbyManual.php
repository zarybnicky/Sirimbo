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
        \Redirect::to('/admin/platby/manual/' . $remaining[0]['pr_id']);
    }

    public static function get($id)
    {
        \Permissions::checkError('platby', P_OWNED);
        $data = \DBPlatbyRaw::getSingle($id);
        $raw = unserialize(stream_get_contents($data['pr_raw']));
        if ($data['pr_sorted']) {
            \Message::info('Platba už byla zařazena do systému');
            \Redirect::to('/admin/platby/manual');
        }

        $categoryLookup = Platby::getCategoryLookup(true, true, false);
        $userLookup = Platby::getUserLookup(false);

        Platby::recognizeHeaders($raw, $specific, $variable, $date, $amount);
        $raw[null] = null;
        $item = new \PlatbyItem($raw[$specific], $raw[$variable], $raw[$date], $raw[$amount]);
        $item->processWithSymbolLookup($userLookup, $categoryLookup);

        $emptyItem = ['column' => '&nbsp;---', 'value' => null];

        if ($variable === null) {
            $recognized['variable'] = $emptyItem;
        } else {
            $recognized['variable'] = [
                'column' => $variable,
                'value' => $item->variable
                ? "{$userLookup[$item->variable]['u_jmeno']} {$userLookup[$item->variable]['u_prijmeni']}"
                : "&nbsp;--- (není v DB: id {$raw[$variable]})",
            ];
        }

        if ($specific === null) {
            $recognized['specific'] = $emptyItem;
        } else {
            $recognized['specific'] = [
                'column' => $specific,
                'value' => $item->specific && $item->categoryId
                ? $item->specific
                : "&nbsp;--- (není v DB: symbol {$raw[$specific]})",
            ];
        }

        $recognized['date'] = $date === null ? $emptyItem : ['column' => $date, 'value' => $item->date];
        $recognized['amount'] = $amount === null ? $emptyItem : ['column' => $amount, 'value' => $raw[$amount]];
        $recognized['prefix'] = ['column' => '&nbsp;---', 'value' => ($item->prefix ? $item->prefix : '&nbsp;---')];

        $remainingCount = count(\DBPlatbyRaw::getUnsorted());
        \Render::twig('Admin/PlatbyManualForm.twig', [
            'id' => $id,
            'remainingTotal' => $remainingCount,
            'raw' => $raw,
            'guess' => [
                'specific' => $item->categoryId,
                'variable' => $item->variable,
                'date' => $item->date,
                'amount' => $item->amount,
                'prefix' => $item->prefix
            ],
            'recognized' => $recognized,
            'users' => self::getUsers(),
            'categories' => self::getCategories(),
        ]);
    }

    public static function post($id)
    {
        \Permissions::checkError('platby', P_OWNED);
        if (!($data = \DBPlatbyRaw::getSingle($id))) {
            \Message::warning('Zadaná platba neexistuje.');
            \Redirect::to('/admin/platby/manual');
        }
        if ($data['pr_sorted']) {
            \Message::info('Zadaná platba už byla zařazená.');
            \Redirect::to('/admin/platby/manual');
        }
        switch ($_POST['action']) {
            case 'confirm':
                if (!is_object($item = Platby::getFromPost())) {
                    \Message::warning($item);
                    return;
                }
                \DBPlatbyRaw::update($id, stream_get_contents($data['pr_raw']), $data['pr_hash'], '1', '0');
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
                \DBPlatbyRaw::update($id, stream_get_contents($data['pr_raw']), $data['pr_hash'], '0', '1');
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
        return array_for(Platby::getUserLookup(true), function ($x) {
            $u = \User::fromArray($x);
            return "{$u->getVarSymbol()} - {$x['u_prijmeni']}, {$x['u_jmeno']}";
        });
    }
}
