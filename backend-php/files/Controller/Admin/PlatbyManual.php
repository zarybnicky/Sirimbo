<?php
namespace Olymp\Controller\Admin;

class PlatbyManual
{
    public static function query()
    {
        \Permissions::checkError('platby', P_OWNED);
        $remaining = \Database::querySingle("SELECT * FROM platby_raw WHERE pr_sorted='0' AND pr_discarded='0' ORDER BY pr_id LIMIT 1");
        if (!$remaining) {
            \Message::info('Nezbývají už žádné nezatříděné platby');
            \Redirect::to('/platby');
        }
        \Redirect::to('/platby/manual/' . $remaining['pr_id']);
    }

    public static function get($id)
    {
        \Permissions::checkError('platby', P_OWNED);
        $data = \Database::querySingle("SELECT * FROM platby_raw WHERE pr_id='?'", $id);
        $raw = unserialize(stream_get_contents($data['pr_raw']));
        if ($data['pr_sorted']) {
            \Message::info('Platba už byla zařazena do systému');
            \Redirect::to('/platby/manual');
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

        $remaining = \Database::queryArray("SELECT * FROM platby_raw WHERE pr_sorted='0' AND pr_discarded='0' ORDER BY pr_id");
        $remainingCount = count($remaining);
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
        $data = \Database::querySingle("SELECT * FROM platby_raw WHERE pr_id='?'", $id);
        if (!$data) {
            \Message::warning('Zadaná platba neexistuje.');
            \Redirect::to('/platby/manual');
        }
        if ($data['pr_sorted']) {
            \Message::info('Zadaná platba už byla zařazená.');
            \Redirect::to('/platby/manual');
        }
        switch ($_POST['action']) {
            case 'confirm':
                if (!is_object($item = Platby::getFromPost())) {
                    \Message::warning($item);
                    return;
                }
                \Database::query("UPDATE platby_raw SET pr_sorted='1', pr_discarded='0' WHERE pr_id='?'", $id);
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
                \Database::query("UPDATE platby_raw SET pr_sorted='0', pr_discarded='1' WHERE pr_id='?'", $id);
                break;
            case 'skip':
                \Database::query(
                    "WITH deletions AS (DELETE FROM platby_raw WHERE pr_id='?' RETURNING pr_raw,pr_hash,pr_sorted,pr_discarded)
                    INSERT INTO platby_raw (pr_raw,pr_hash,pr_sorted,pr_discarded) SELECT * from deletions",
                    $id
                );
                break;
            default:
                \Message::danger('Neplatná POST akce.');
                break;
        }
        \Redirect::to('/platby/manual');
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
            function ($x) {
                $u = \User::fromArray($x);
                return "{$u->getVarSymbol()} - {$x['u_prijmeni']}, {$x['u_jmeno']}";
            },
            Platby::getUserLookup(true),
        );
    }
}
