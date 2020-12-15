<?php
class Controller_Admin_Platby_Structure_Category extends Controller_Admin_Platby
{
    public function view($request)
    {
        \Permissions::checkError('platby', P_OWNED);
        if ($id = $_POST['category_duplicate']) {
            if (!($data = \DBPlatbyCategory::getSingle($id))) {
                new \MessageHelper('warning', 'Takový specifický symbol neexistuje.');
                new \RedirectHelper('/admin/platby/structure/category');
            }

            \DBPlatbyCategory::insert(
                $data['pc_name'],
                $data['pc_symbol'] . '00',
                $data['pc_amount'],
                $data['pc_date_due'],
                $data['pc_valid_from'],
                $data['pc_valid_to'],
                $data['pc_use_base'],
                $data['pc_use_prefix'],
                $data['pc_archive'],
                $data['pc_visible']
            );
            $insertId = \DBPlatbyCategory::getInsertId();

            $groups = \DBPlatbyCategory::getSingleWithGroups($id);
            foreach ($groups as $group) {
                \DBPlatbyGroup::addChild($group['pg_id'], $insertId);
            }
        }

        new \RenderHelper('files/View/Admin/Platby/StructureSymbolOverview.inc', [
            'header' => 'Správa plateb',
            'subheader' => 'Specifické symboly',
            'data' => static::getCategories(false),
            'archived' => static::getCategories(true),
            'uri' => trim(explode('?', $_SERVER['REQUEST_URI'])[0], '/')
        ]);
    }

    protected static function getCategories($archived = false)
    {
        return array_map(
            fn($item) => [
                'name' => $item['pc_name'],
                'symbol' => $item['pc_symbol'],
                'amount' => ($item['pc_amount'] . ($item['pc_use_base'] ? ' * ?' : '')) . ' Kč',
                'validDate' => formatRange($item['pc_valid_from'], $item['pc_valid_to']),
                'buttons' => (
                    new \EditLinkHelper(
                        '/admin/platby/structure/category/edit/' . $item['pc_id']
                    ) . '&nbsp;' . (new \SubmitHelper(
                        '<img title="Duplikovat" alt="Duplikovat" src="/style/icon-files-o.png" />'
                    ))->data('category_duplicate', $item['pc_id'])->cls('btn btn-link btn-sm')
                    . '&nbsp;' . new \RemoveLinkHelper(
                        '/admin/platby/structure/category/remove/' . $item['pc_id']
                    )
                )
            ],
            \DBPlatbyCategory::get($archived)
        );
    }

    public function add($request)
    {
        \Permissions::checkError('platby', P_OWNED);
        if (!$_POST) {
            return static::displayForm($request, 'add', 0);
        }
        $form = static::checkData($request, 'add', 0);
        if (!$form->isValid()) {
            new \MessageHelper('warning', $form->getMessages());
            return static::displayForm($request, 'add', 0);
        }

        $validRange = \DateHelper::getPostRange('validRange');
        $validFrom = $validRange['from'];
        $validTo = $validRange['to'];
        if (!$validTo->isValid()) {
            $validTo = $validFrom;
        } elseif (strcasecmp((string) $validFrom, (string) $validTo) > 0) {
            $validFrom = $validTo;
        }

        $amount = $_POST['amount'];
        $use_base = '0';
        if (strpos($amount, '*') !== false) {
            $use_base = '1';
            $amount = str_replace('*', '', $amount);
        }

        \DBPlatbyCategory::insert(
            $_POST['name'],
            $_POST['symbol'],
            $amount,
            (string) new \Date($_POST['dueDate'] ?? null),
            (string) $validFrom,
            (string) $validTo,
            $use_base,
            $_POST['usePrefix'] ? '1' : '0',
            $_POST['archive'] ? '1' : '0',
            $_POST['visible'] ? '1' : '0'
        );
        $insertId = \DBPlatbyCategory::getInsertId();

        foreach ($_POST['group'] ?: [] as $item) {
            \DBPlatbyGroup::addChild($item, $insertId);
        }

        new \RedirectHelper($_POST['returnURI'] ?: '/admin/platby/structure/category');
    }

    public function edit($request)
    {
        \Permissions::checkError('platby', P_OWNED);
        if (!$id = $request->getId()) {
            new \MessageHelper('warning', 'Kategorie s takovým ID neexistuje');
            new \RedirectHelper($_POST['returnURI'] ?: '/admin/platby/structure/category');
        }
        if (!$data = \DBPlatbyCategory::getSingle($id)) {
            new \MessageHelper('warning', 'Kategorie s takovým ID neexistuje');
            new \RedirectHelper($_POST['returnURI'] ?: '/admin/platby/structure/category');
        }

        if (!$_POST) {
            if ($data['pc_use_base']) {
                $data['pc_amount'] = '*' . $data['pc_amount'];
            }
            $_POST['name'] = $data['pc_name'];
            $_POST['symbol'] = $data['pc_symbol'];
            $_POST['amount'] = $data['pc_amount'];
            $_POST['dueDate'] = $data['pc_date_due'];
            $_POST['validRange'] = $data['pc_valid_from'] . ' - ' . $data['pc_valid_to'];
            $_POST['usePrefix'] = $data['pc_use_prefix'];
            $_POST['archive'] = $data['pc_archive'];
            $_POST['visible'] = $data['pc_visible'];
            return static::displayForm($request, 'edit', $id);
        }
        $form = static::checkData($request, 'edit', $id);
        if (!$form->isValid()) {
            new \MessageHelper('warning', $form->getMessages());
            return static::displayForm($request, 'edit', $id);
        }

        $validRange = \DateHelper::getPostRange('validRange');
        $validFrom = $validRange['from'];
        $validTo = $validRange['to'];
        if (!$validTo->isValid()) {
            $validTo = $validFrom;
        } elseif (strcasecmp((string) $validFrom, (string) $validTo) > 0) {
            $validFrom = $validTo;
        }

        $amount = $_POST['amount'];
        $use_base = '0';
        if (strpos($amount, '*') !== false) {
            $use_base = '1';
            $amount = str_replace('*', '', $amount);
        }
        $use_prefix = $_POST['usePrefix'] ? '1' : '0';
        $archive = $_POST['archive'] ? '1' : '0';
        $visible = $_POST['visible'] ? '1' : '0';

        \DBPlatbyCategory::update(
            $id,
            $_POST['name'],
            $_POST['symbol'],
            $amount,
            (string) new \Date($_POST['dueDate'] ?? null),
            (string) $validFrom,
            (string) $validTo,
            $use_base,
            $use_prefix,
            $archive,
            $visible
        );

        $groupsOld = array_map(
            fn($item) => $item['pg_id'],
            \DBPlatbyCategory::getSingleWithGroups($id)
        );
        $groupsNew = $_POST['group'] ?: [];
        foreach (array_diff($groupsOld, $groupsNew) as $removed) {
            \DBPlatbyGroup::removeChild($removed, $id);
        }
        foreach (array_diff($groupsNew, $groupsOld) as $added) {
            \DBPlatbyGroup::addChild($added, $id);
        }

        if ($_GET['group']) {
            new \RedirectHelper('/admin/platby/structure/group/edit/' . $_GET['group']);
        }
        new \RedirectHelper($_POST['returnURI'] ?: '/admin/platby/structure/category');
    }

    public function remove($request)
    {
        \Permissions::checkError('platby', P_OWNED);
        if (!$id = $request->getId()) {
            new \MessageHelper('warning', 'Specifický symbol s takovým ID neexistuje');
            new \RedirectHelper($_POST['returnURI'] ?: '/admin/platby/structure/category');
        }
        if (!$data = \DBPlatbyCategory::getSingle($id)) {
            new \MessageHelper('warning', 'Specifický symbol s takovým ID neexistuje');
            new \RedirectHelper($_POST['returnURI'] ?: '/admin/platby/structure/category');
        }

        if ($_POST['action'] == 'unlink') {
            $f = static::getLinkedObjects($id);

            $groupCount = count($f['groups']);
            foreach ($f['groups'] as $data) {
                \DBPlatbyGroup::removeChild($data['pg_id'], $id);
            }

            $itemCount = count($f['items']);
            foreach ($f['items'] as $data) {
                $raw = \DBPlatbyRaw::getSingle($data['pi_id_raw']);
                \DBPlatbyRaw::update(
                    $raw['pr_id'],
                    $raw['pr_raw'],
                    $raw['pr_hash'],
                    '0',
                    '0'
                );
                \DBPlatbyItem::remove($data['pi_id']);
            }
            new \MessageHelper('info', "Spojení s $groupCount kategoriemi a s $itemCount platbami bylo odstraněno");
            return new \RedirectHelper('/admin/platby/structure/category/remove/' . $id);
        } elseif ($_POST['action'] == 'archive') {
            \DBPlatbyCategory::update(
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
            new \MessageHelper('info', 'Specifický symbol "' . $data['pc_symbol'] . '" byl archivován');
            return new \RedirectHelper('/admin/platby/structure/category');
        }
        if (((!$_POST || $_POST['action'] == 'confirm')
            && ($f = static::getLinkedObjects($id)))
            || !$_POST
        ) {
            if (isset($f) && $f) {
                new \MessageHelper(
                    'info',
                    'Nemůžu odstranit specifický symbol s připojenými kategoriemi nebo položkami! '
                    . new Tag(
                        'form',
                        ['action' => '', 'method' => 'post'],
                        (!$data['pc_archive']
                         ? ((new \SubmitHelper('Archivovat?'))->data('action', 'archive') . ' nebo ')
                         : ''),
                         (new \SubmitHelper('Odstranit všechna spojení se skupinami a kategoriemi a přesunout ovlivněné platby do nezařazených?'))->data('action', 'unlink')
                    )
                );
            }
            return new \RenderHelper('files/View/Admin/Platby/StructureSymbolRemove.inc', [
                'header' => 'Správa plateb',
                'subheader' => 'Specifické symboly_',
                'id' => $id,
                'name' => $data['pc_name'],
                'returnURI' => $_SERVER['HTTP_REFERER'],
                'uri' => trim(explode('?', $_SERVER['REQUEST_URI'])[0], '/')
            ]);
        }
        \DBPlatbyCategory::delete($id);
        new \RedirectHelper($_POST['returnURI'] ?: '/admin/platby/structure/category');
    }

    protected static function getLinkedObjects($id)
    {
        $group = \DBPlatbyCategory::getSingleWithGroups($id);
        $items = \DBPlatbyItem::get(true, ['pc_id' => $id]);
        return ($group || $items) ? ['groups' => $group, 'items' => $items] : [];
    }

    protected static function displayForm($request, $action, $id = 0)
    {
        $groupsSelected = array_flip(
            array_map(
                fn($item) => $item['pg_id'],
                \DBPlatbyCategory::getSingleWithGroups($id)
            )
        );
        $groups = array_map(
            fn($item) => [
                'buttons' => new \CheckboxHelper('group[]', $item['pg_id'], isset($groupsSelected[$item['pg_id']])),
                'type' => ($item['pg_type'] == '1' ? 'Členské příspěvky' : 'Běžné platby'),
                'name' => $item['pg_name'],
                'base' => $item['pg_base']
            ],
            \DBPlatbyGroup::getGroups()
        );

        new \RenderHelper('files/View/Admin/Platby/StructureSymbolForm.inc', [
            'header' => 'Správa plateb',
            'subheader' => ($action == 'add' ? 'Přidat' : 'Upravit') . ' specifický symbol',
            'id' => $id,
            'action' => $action,
            'groups' => $groups,
            'returnURI' => $_SERVER['HTTP_REFERER'],
            'name' => $_POST['name'] ?: '',
            'symbol' => $_POST['symbol'] ?: '',
            'amount' => $_POST['amount'] ?: '',
            'dueDate' => $_POST['dueDate'] ?: '',
            'validRange' => $_POST['validRange'] ?: '',
            'usePrefix' => $_POST['usePrefix'] ?: '',
            'archive' => $_POST['archive'] ?: '',
            'visible' => $_POST['visible'] ?: '',
            'uri' => trim(explode('?', $_SERVER['REQUEST_URI'])[0], '/')
        ]);
    }

    protected function checkData($request, $action, $id): \Form
    {
        $f = new \Form();
        $dueDate = new \Date($_POST['dueDate'] ?? null);
        if ($dueDate->getYear() == '0000') {
            $dueDate = str_replace('0000', '2000', (string) $dueDate);
        }
        $f->checkDate($dueDate, 'Datum splatnosti není platné.');

        $validRange = \DateHelper::getPostRange('validRange');
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
        if (!$_POST['archive']) {
            $f->checkBool(
                !($active = \DBPlatbyCategory::checkActiveSymbol($_POST['symbol']))
                || ($action == 'edit' ? $active['pc_id'] == $id : false),
                $active ? ('Už existuje aktivní specifický symbol se symbolem ' . $_POST['symbol'] . ' (' . $active['pc_name'] . ')') : '', ''
            );
        }
        $f->checkNotEmpty($_POST['name'], 'Zadejte prosím nějaké jméno.');
        $f->checkNumeric($_POST['symbol'], 'Zadejte prosím platný specifický symbol.');
        $f->checkRegexp($_POST['amount'], '/(\*)?([0-9]+)([.,][0-9]+)?/', 'Zadejte prosím platnou očekávanou částku.');

        return $f;
    }
}
