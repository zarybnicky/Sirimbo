<?php
class Controller_Admin_Platby extends Controller_Abstract
{
    public function __construct()
    {
        Permissions::checkError('platby', P_OWNED);
    }

    public function view($request)
    {
        $this->redirect('/admin/platby/overview');
    }

    protected function recognizeHeaders($headers, &$specific, &$variable, &$date, &$amount)
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

    protected function checkHeaders($headers, &$specific, &$variable, &$date, &$amount)
    {
        $headers = array_flip($headers);
        return isset($headers[$specific])
            && isset($headers[$variable])
            && isset($headers[$date])
            && isset($headers[$amount]);
    }

    protected function getCategoryList()
    {
        $in = DBPlatbyGroup::getGroupsWithCategories();
        $out = [];
        $group_id = 0;
        foreach ($in as $array) {
            if ($group_id != $array['pg_id']
                && !isset($out['group_' . $array['pg_id']])
            ) {
                $out[] = ['group_' . $array['pg_id'], $array];
                $group_id = $array['pg_id'];
            }
            $out[] = [$array['pc_id'], $array];
        }
        return $out;
    }

    protected function getCategoryLookup($useSymbolKey, $unique, $includeGroups)
    {
        $in = DBPlatbyGroup::getGroupsWithCategories();
        $out = [];
        $group_id = 0;
        foreach ($in as $array) {
            $key = (int) ($useSymbolKey
                          ? $array['pc_symbol']
                          : $array['pc_id']);

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
        $out = [];
        foreach ($in as $array) {
            $out[(int) $array['u_id']] = $array;
        }
        return $out;
    }

    protected function getFromPost($request, $id = null)
    {
        $item = new PlatbyItem();
        $item->init(
            null,
            $request->post('variable'),
            $request->post('date'),
            $request->post('amount'),
            $request->post('prefix'),
            $id,
            $request->post('specific')
        );
        $item->processWithSymbolLookup(
            $this->getUserLookup(false),
            $this->getCategoryLookup(true, true, false)
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