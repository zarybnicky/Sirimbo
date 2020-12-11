<?php
class Controller_Admin_Platby_Manual extends Controller_Admin_Platby
{
    public function __construct($request)
    {
        parent::__construct($request);
        Permissions::checkError('platby', P_OWNED);
    }

    public function view($request)
    {
        if ($request->post()) {
            $this->processPost($request);
        }
        $remaining = DBPlatbyRaw::getUnsorted();
        $remainingCount = count($remaining);

        $id = $request->getId();
        if ($id && ($data = DBPlatbyRaw::getSingle($id))) {
            if ($data['pr_sorted']) {
                $this->redirect()->info('Platba už byla zařazena do systému');
                $this->redirect('/admin/platby/discarded');
            }
            $raw = unserialize($data['pr_raw']);
        } else {
            if ($remainingCount == 0) {
                $this->redirect()->info('Nezbývají už žádné nezatříděné platby');
                $this->redirect('/admin/platby');
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
                'value' => (new Date($item->date))->getDate(Date::FORMAT_SIMPLIFIED)
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

        $this->render('files/View/Admin/Platby/ManualForm.inc', [
            'header' => 'Správa plateb',
            'subheader' => 'Ruční třídění plateb</span> (zbývá ' . $remainingCount . ')',
            'id' => $id,
            'remainingTotal' => $remainingCount,
            'raw' => $raw,
            'guess' => [
                'specific' => $item->categoryId,
                'variable' => $item->variable,
                'date' => (new Date($item->date))->getDate(Date::FORMAT_SIMPLIFIED),
                'amount' => $item->amount,
                'prefix' => $item->prefix
            ],
            'users' => $this->getUsers(),
            'categories' => $this->getCategories(),
            'recognized' => $recognized,
            'uri' => $request->getLiteralURI()
        ]);
    }

    private function getCategories()
    {
        $categories = parent::getCategoryLookup(false, false, true);
        $res = [];
        foreach ($categories as $key => $array) {
            if (strpos($key, 'group_') === false) {
                $res[$key] = "{$array['pc_symbol']} - {$array['pc_name']}";
            }
        }
        return $res;
    }

    private function getUsers()
    {
        $users = parent::getUserLookup(true);
        foreach ($users as &$array) {
            $array = User::varSymbol($array['u_id']) . " - {$array['u_prijmeni']}, {$array['u_jmeno']}";
        }
        return $users;
    }

    private function processPost($request)
    {
        $id = $request->post('id');
        if (!$id || !($current = DBPlatbyRaw::getSingle($id))) {
            $this->redirect()->warning('Zadaná platba neexistuje.');
            return;
        } elseif ($current['pr_sorted'] && ($item = DBPlatbyItem::getSingleByRawId($id))) {
            $this->redirect()->info('Zadaná platba už byla zařazená.');
            return;
        }

        switch ($request->post('action')) {
            case 'confirm':
                if (!is_object($item = $this->getFromPost($request))) {
                    $this->redirect()->warning($item);
                    return;
                }
                DBPlatbyRaw::update(
                    $id,
                    $current['pr_raw'],
                    $current['pr_hash'],
                    '1',
                    '0'
                );
                DBPlatbyItem::insert(
                    $item->variable,
                    $item->categoryId,
                    $id,
                    $item->amount,
                    $item->date,
                    $item->prefix
                );
                break;

            case 'discard':
                DBPlatbyRaw::update(
                    $id,
                    $current['pr_raw'],
                    $current['pr_hash'],
                    '0',
                    '1'
                );
                break;

            case 'skip':
                DBPlatbyRaw::skip($id);
                break;

            default:
                $this->redirect()->danger('Neplatná POST akce.');
                break;
        }

        $this->redirect('/admin/platby/manual');
    }
}
