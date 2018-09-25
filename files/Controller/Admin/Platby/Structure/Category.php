<?php
require_once 'files/Controller/Admin/Platby/Structure.php';
class Controller_Admin_Platby_Structure_Category extends Controller_Admin_Platby_Structure
{
    public function __construct()
    {
        Permissions::checkError('platby', P_OWNED);
    }
    public function view($request)
    {
        if ($id = $request->post('category_duplicate')) {
            if (!($data = DBPlatbyCategory::getSingle($id))) {
                $this->redirect(
                    '/admin/platby/structure/category',
                    'Takový specifický symobl neexistuje.'
                );
            }

            DBPlatbyCategory::insert(
                $data['pc_name'],
                'NULL',
                $data['pc_amount'],
                $data['pc_date_due'],
                $data['pc_valid_from'],
                $data['pc_valid_to'],
                $data['pc_use_base'],
                $data['pc_use_prefix'],
                $data['pc_archive'],
                $data['pc_visible']
            );
            $insertId = DBPlatbyCategory::getInsertId();

            $groups = DBPlatbyCategory::getSingleWithGroups($id);
            foreach ($groups as $group) {
                DBPlatbyGroup::addChild($group['pg_id'], $insertId);
            }
        }

        $this->render(
            'files/View/Admin/Platby/StructureSymbolOverview.inc',
            [
                'data' => $this->getCategories(false),
                'archived' => $this->getCategories(true),
                'uri' => $request->getLiteralURI()
            ]
        );
    }

    protected function getCategories($archived = false)
    {
        return array_map(
            function ($item) {
                return [
                    'name' => $item['pc_name'],
                    'symbol' => $item['pc_symbol'],
                    'amount' => ($item['pc_amount'] . ($item['pc_use_base'] ? ' * ?' : '')) . ' Kč',
                    'validDate' => $this->getDateDisplay($item['pc_valid_from'], $item['pc_valid_to']),
                    'buttons' => (
                        $this->editLink('/admin/platby/structure/category/edit/' . $item['pc_id'])
                        . $this->getDuplicateCategoryButton($item['pc_id'])
                        . $this->removeLink('/admin/platby/structure/category/remove/' . $item['pc_id'])
                    )
                ];
            },
            DBPlatbyCategory::get($archived)
        );
    }

    public function add($request)
    {
        if (!$request->post() || is_object($s = $this->checkPost($request, 'add', 0))) {
            if ($request->post()) {
                $this->redirect()->setMessage($s->getMessages());
            }
            $this->displayForm($request, 'add', 0);
            return;
        }

        $validRange = $this->date('validRange')->getPostRange($request);
        $validFrom = $validRange['from'];
        $validTo = $validRange['to'];
        if (!$validTo->isValid()) {
            $validTo = $validFrom;
        } elseif (strcasecmp((string) $validFrom, (string) $validTo) > 0) {
            $validFrom = $validTo;
        }

        $amount = $request->post('amount');
        $use_base = '0';
        if (strpos($amount, '*') !== false) {
            $use_base = '1';
            $amount = str_replace('*', '', $amount);
        }

        DBPlatbyCategory::insert(
            $request->post('name'),
            $request->post('symbol'),
            $amount,
            (string) $this->date('dueDate')->getPost($request),
            (string) $validFrom,
            (string) $validTo,
            $use_base,
            $request->post('usePrefix') ? '1' : '0',
            $request->post('archive') ? '1' : '0',
            $request->post('visible') ? '1' : '0'
        );
        $insertId = DBPlatbyCategory::getInsertId();

        foreach ($request->post('group') ?: [] as $item) {
            DBPlatbyGroup::addChild($item, $insertId);
        }

        $this->redirect(
            $request->post('referer') ?: '/admin/platby/structure/category',
            'Specifický symbol úspěšně přidán'
        );
    }

    public function edit($request)
    {
        $id = $request->getId();
        if (!$id || !($data = DBPlatbyCategory::getSingle($id))) {
            $this->redirect(
                $request->post('referer') ?: '/admin/platby/structure/category',
                'Kategorie s takovým ID neexistuje'
            );
        }

        if (!$request->post() || is_object($s = $this->checkPost($request, 'edit', $id))) {
            if ($request->post()) {
                $this->redirect()->setMessage($s->getMessages());
            } else {
                if ($data['pc_use_base']) {
                    $data['pc_amount'] = '*' . $data['pc_amount'];
                }
                $request->post('name', $data['pc_name']);
                $request->post('symbol', $data['pc_symbol']);
                $request->post('amount', $data['pc_amount']);
                $request->post('dueDate', $data['pc_date_due']);
                $request->post('validRange', $data['pc_valid_from'] . ' - ' . $data['pc_valid_to']);
                $request->post('usePrefix', $data['pc_use_prefix']);
                $request->post('archive', $data['pc_archive']);
                $request->post('visible', $data['pc_visible']);
            }
            $this->displayForm($request, 'edit', $id);
            return;
        }
        $dueDate = $this->date('dueDate')->getPost($request);

        $validRange = $this->date('validRange')->range()->getPostRange($request);
        $validFrom = $validRange['from'];
        $validTo = $validRange['to'];
        if (!$validTo->isValid()) {
            $validTo = $validFrom;
        } elseif (strcasecmp((string) $validFrom, (string) $validTo) > 0) {
            $validFrom = $validTo;
        }

        $amount = $request->post('amount');
        $use_base = '0';
        if (strpos($amount, '*') !== false) {
            $use_base = '1';
            $amount = str_replace('*', '', $amount);
        }
        $use_prefix = $request->post('usePrefix') ? '1' : '0';
        $archive = $request->post('archive') ? '1' : '0';
        $visible = $request->post('visible') ? '1' : '0';

        DBPlatbyCategory::update(
            $id,
            $request->post('name'),
            $request->post('symbol'),
            $amount,
            (string) $dueDate,
            (string) $validFrom,
            (string) $validTo,
            $use_base,
            $use_prefix,
            $archive,
            $visible
        );

        $groupsOld = array_map(
            function ($item) {
                return $item['pg_id'];
            },
            DBPlatbyCategory::getSingleWithGroups($id)
        );
        $groupsNew = $request->post('group') ?: [];
        foreach (array_diff($groupsOld, $groupsNew) as $removed) {
            DBPlatbyGroup::removeChild($removed, $id);
        }
        foreach (array_diff($groupsNew, $groupsOld) as $added) {
            DBPlatbyGroup::addChild($added, $id);
        }

        if ($request->get('group')) {
            $this->redirect(
                '/admin/platby/structure/group/edit/' . $request->get('group'),
                'Specifický symbol úspěšně upraven'
            );
        }
        $this->redirect(
            $request->post('referer') ?: '/admin/platby/structure/category',
            'Specifický symbol úspěšně upraven'
        );
    }

    public function remove($request)
    {
        $id = $request->getId();
        if (!$id || !($data = DBPlatbyCategory::getSingle($id))) {
            $this->redirect(
                $request->post('referer') ?: '/admin/platby/structure/category',
                'Specifický symbol s takovým ID neexistuje'
            );
        }

        if ($request->post('action') == 'unlink') {
            $f = $this->getLinkedObjects($id);

            $groupCount = count($f['groups']);
            foreach ($f['groups'] as $data) {
                DBPlatbyGroup::removeChild($data['pg_id'], $id);
            }

            $itemCount = count($f['items']);
            foreach ($f['items'] as $data) {
                $raw = DBPlatbyRaw::getSingle($data['pi_id_raw']);
                DBPlatbyRaw::update(
                    $raw['pr_id'],
                    $raw['pr_raw'],
                    $raw['pr_hash'],
                    '0',
                    '0'
                );
                DBPlatbyItem::remove($data['pi_id']);
            }
            $this->redirect(
                '/admin/platby/structure/category/remove/' . $id,
                "Spojení s $groupCount kategoriemi a s $itemCount platbami bylo odstraněno"
            );
            return;
        } elseif ($request->post('action') == 'archive') {
            DBPlatbyCategory::update(
                $id,
                $data['pc_name'],
                $data['pc_symbol'],
                $data['pc_amount'],
                $data['pc_date_due'],
                $data['pc_valid_from'],
                $data['pc_valid_to'],
                $data['pc_use_base'],
                $data['pc_use_prefix'],
                '1',
                $data['pc_visible']
            );
            $this->redirect(
                '/admin/platby/structure/category',
                'Specifický symbol "' . $data['pc_symbol'] . '" byl archivován'
            );
            return;
        }
        if (((!$request->post() || $request->post('action') == 'confirm')
            && ($f = $this->getLinkedObjects($id)))
            || !$request->post()
        ) {
            if (isset($f) && $f) {
                $this->redirect()->setMessage(
                    'Nemůžu odstranit specifický symbol s připojenými kategoriemi nebo položkami! '
                    . new Tag(
                        'form',
                        ['action' => '', 'method' => 'post'],
                        (!$data['pc_archive']
                         ? ($this->submit('Archivovat?')->data('action', 'archive') . ' nebo ')
                         : ''),
                        (string) $this->submit('Odstranit všechna spojení se skupinami a kategoriemi a přesunout ovlivněné platby do nezařazených?')->data('action', 'unlink')
                    )
                );
            }
            $this->render(
                'files/View/Admin/Platby/StructureSymbolRemove.inc',
                [
                    'id' => $id,
                    'name' => $data['pc_name'],
                    'referer' => $request->getReferer(),
                    'uri' => $request->getLiteralURI()
                ]
            );
            return;
        }
        DBPlatbyCategory::delete($id);
        $this->redirect(
            $request->post('referer') ?: '/admin/platby/structure/category',
            'Specifický symbol byl odebrán'
        );
    }

    protected function getLinkedObjects($id)
    {
        $group = DBPlatbyCategory::getSingleWithGroups($id);
        $items = DBPlatbyItem::get(true, ['pc_id' => $id]);

        return ($group || $items)
            ? ['groups' => $group, 'items' => $items]
            : [];
    }

    protected function displayForm($request, $action, $id = 0)
    {
        $groupsSelected = array_flip(
            array_map(
                function ($item) {
                    return $item['pg_id'];
                },
                DBPlatbyCategory::getSingleWithGroups($id)
            )
        );
        $groups = array_map(
            function ($item) use ($groupsSelected) {
                return [
                    'buttons' => $this->checkbox('group[]', $item['pg_id'])
                                      ->set(isset($groupsSelected[$item['pg_id']]))
                                      ->render(),
                    'type' => ($item['pg_type'] == '1' ? 'Členské příspěvky' : 'Běžné platby'),
                    'name' => $item['pg_name'],
                    'base' => $item['pg_base']
                ];
            },
            DBPlatbyGroup::getGroups()
        );

        $this->render(
            'files/View/Admin/Platby/StructureSymbolForm.inc',
            [
                'id' => $id,
                'action' => $action,
                'groups' => $groups,
                'referer' => $request->getReferer(),
                'name' => $request->post('name') ?: '',
                'symbol' => $request->post('symbol') ?: '',
                'amount' => $request->post('amount') ?: '',
                'dueDate' => $request->post('dueDate') ?: '',
                'validRange' => $request->post('validRange') ?: '',
                'usePrefix' => $request->post('usePrefix') ?: '',
                'archive' => $request->post('archive') ?: '',
                'visible' => $request->post('visible') ?: '',
                'uri' => $request->getLiteralURI()
            ]
        );
    }

    protected function checkPost($request, $action, $id)
    {
        $f = new Form();
        $dueDate = $this->date('dueDate')->getPost($request);
        if ($dueDate->getYear() == '0000') {
            $dueDate = str_replace('0000', '2000', (string) $dueDate);
        }
        $f->checkDate($dueDate, 'Datum splatnosti není platné.');

        $validRange = $this->date('validRange')->range()->getPostRange($request);
        if ($validRange['from']->getYear() == '0000') {
            $f->checkDate(
                str_replace('0000', '2000', (string) $validRange['from']),
                'Datum platnosti není platné'
            );
            if ($validRange['to']->isValid() && $validRange['to']->getYear() == '0000') {
                $f->checkDate(
                    str_replace('0000', '2000', (string) $validRange['to']),
                    'Datum platnosti (část \'do\') není platné'
                );
            } else {
                $f->checkDate(
                    (string) $validRange['from'],
                    'Datum platnosti (část \'do\') není platné'
                );
            }
        } else {
            $f->checkDate(
                (string) $validRange['from'],
                'Datum platnosti není platné'
            );
            if ($validRange['to']->isValid()) {
                $f->checkDate(
                    (string) $validRange['from'],
                    'Datum platnosti (část \'do\') není platné'
                );
            }
        }
        if (!$request->post('archive')) {
            $f->checkBool(
                !($active = DBPlatbyCategory::checkActiveSymbol($request->post('symbol')))
                || ($action == 'edit' ? $active['pc_id'] == $id : false),
                $active ? ('Už existuje aktivní specifický symbol se symbolem ' . $request->post('symbol') . ' (' . $active['pc_name'] . ')') : '', ''
            );
        }
        $f->checkNotEmpty($request->post('name'), 'Zadejte prosím nějaké jméno.');
        $f->checkNumeric($request->post('symbol'), 'Zadejte prosím platný specifický symbol.');
        $f->checkRegexp($request->post('amount'), '/(\*)?([0-9]+)([.,][0-9]+)?/', 'Zadejte prosím platnou očekávanou částku.');

        return $f->isValid() ? true : $f;
    }
}
