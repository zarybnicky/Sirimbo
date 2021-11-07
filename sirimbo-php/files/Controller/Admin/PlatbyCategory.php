<?php
namespace Olymp\Controller\Admin;

class PlatbyCategory
{
    public static function list()
    {
        \Permissions::checkError('platby', P_OWNED);
        \Render::twig('Admin/PlatbyStructureSymbol.twig', [
            'data' => \DBPlatbyCategory::get(false),
            'archived' => \DBPlatbyCategory::get(true),
        ]);
    }

    public static function listPost()
    {
        if (!$id = $_POST['category_duplicate']) {
            \Redirect::to('/admin/platby/structure/category');
        }
        if (!($data = \DBPlatbyCategory::getSingle($id))) {
            \Message::warning('Takový specifický symbol neexistuje.');
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
        foreach (\DBPlatbyCategory::getSingleWithGroups($id) as $group) {
            \DBPlatbyGroup::addChild($group['pg_id'], $insertId);
        }
        \Redirect::to('/admin/platby/structure/category');
    }

    public static function add()
    {
        \Permissions::checkError('platby', P_OWNED);
        return static::displayForm('add', 0);
    }

    public static function addPost()
    {
        \Permissions::checkError('platby', P_OWNED);
        $form = static::checkData('add', 0);
        if (!$form->isValid()) {
            \Message::warning($form->getMessages());
            return static::displayForm('add', 0);
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
        foreach ($_POST['group'] ?? [] as $item) {
            \DBPlatbyGroup::addChild($item, $insertId);
        }
        \Redirect::to($_POST['returnURI'] ?? '/admin/platby/structure/category');
    }

    public static function edit($id)
    {
        \Permissions::checkError('platby', P_OWNED);
        if (!$data = \DBPlatbyCategory::getSingle($id)) {
            \Message::warning('Kategorie s takovým ID neexistuje');
            \Redirect::to($_POST['returnURI'] ?? '/admin/platby/structure/category');
        }
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
        return static::displayForm('edit', $id);
    }

    public static function editPost($id)
    {
        \Permissions::checkError('platby', P_OWNED);
        if (!\DBPlatbyCategory::getSingle($id)) {
            \Message::warning('Kategorie s takovým ID neexistuje');
            \Redirect::to($_POST['returnURI'] ?? '/admin/platby/structure/category');
        }
        $form = static::checkData('edit', $id);
        if (!$form->isValid()) {
            \Message::warning($form->getMessages());
            return static::displayForm('edit', $id);
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

        $groupsOld = array_column(\DBPlatbyCategory::getSingleWithGroups($id), 'pg_id');
        $groupsNew = $_POST['group'] ?? [];
        foreach (array_diff($groupsOld, $groupsNew) as $removed) {
            \DBPlatbyGroup::removeChild($removed, $id);
        }
        foreach (array_diff($groupsNew, $groupsOld) as $added) {
            \DBPlatbyGroup::addChild($added, $id);
        }
        if ($_GET['group']) {
            \Redirect::to('/admin/platby/structure/group/edit/' . $_GET['group']);
        }
        \Redirect::to($_POST['returnURI'] ?? '/admin/platby/structure/category');
    }

    public static function remove($id)
    {
        \Permissions::checkError('platby', P_OWNED);
        if (!$data = \DBPlatbyCategory::getSingle($id)) {
            \Message::warning('Specifický symbol s takovým ID neexistuje');
            \Redirect::to($_POST['returnURI'] ?? '/admin/platby/structure/category');
        }
        if (static::getLinkedObjects($id)) {
            \Message::info(
                'Nemůžu odstranit specifický symbol s připojenými kategoriemi nebo položkami! '
                . '<form method="post">'
                . (!$data['pc_archive']
                   ? '<button class="btn btn-primary" name="action" value="archive">Archivovat?</button> nebo '
                   : '')
                . '<button class="btn btn-primary" name="action" value="unlink">'
                . 'Odstranit všechna spojení se skupinami a kategoriemi a přesunout ovlivněné platby do nezařazených?'
                . '</button>'
            );
        }
        \Render::twig('Admin/PlatbyStructureSymbolRemove.twig', [
            'id' => $id,
            'name' => $data['pc_name'],
            'returnURI' => $_SERVER['HTTP_REFERER'],
        ]);
    }

    public static function removePost($id)
    {
        \Permissions::checkError('platby', P_OWNED);
        if (!$data = \DBPlatbyCategory::getSingle($id)) {
            \Message::warning('Specifický symbol s takovým ID neexistuje');
            \Redirect::to($_POST['returnURI'] ?? '/admin/platby/structure/category');
        }

        $f = static::getLinkedObjects($id);
        if ($_POST['action'] == 'unlink') {
            $groupCount = count($f['groups']);
            foreach ($f['groups'] as $data) {
                \DBPlatbyGroup::removeChild($data['pg_id'], $id);
            }

            $itemCount = count($f['items']);
            foreach ($f['items'] as $data) {
                $raw = \DBPlatbyRaw::getSingle($data['pi_id_raw']);
                \DBPlatbyRaw::update(
                    $raw['pr_id'],
                    stream_get_contents($raw['pr_raw']),
                    $raw['pr_hash'],
                    '0',
                    '0'
                );
                \DBPlatbyItem::remove($data['pi_id']);
            }
            \Message::info("Spojení s $groupCount kategoriemi a s $itemCount platbami bylo odstraněno");
            \Redirect::to('/admin/platby/structure/category/remove/' . $id);
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
            \Message::info('Specifický symbol "' . $data['pc_symbol'] . '" byl archivován');
            \Redirect::to('/admin/platby/structure/category');
        }
        if ($f) {
            \Redirect::to('/admin/platby/structure/category/remove/' . $id);
        }
        \DBPlatbyCategory::delete($id);
        \Redirect::to($_POST['returnURI'] ?? '/admin/platby/structure/category');
    }

    protected static function getLinkedObjects($id)
    {
        $group = \DBPlatbyCategory::getSingleWithGroups($id);
        $items = \DBPlatbyItem::get(true, ['pc_id' => $id]);
        return ($group || $items) ? ['groups' => $group, 'items' => $items] : [];
    }

    protected static function displayForm($action, $id = 0)
    {
        \Render::twig('Admin/PlatbyStructureSymbolForm.twig', [
            'id' => $id,
            'action' => $action,
            'groups' => \DBPlatbyGroup::getGroups(),
            'groupsSelected' => array_flip(array_column(\DBPlatbyCategory::getSingleWithGroups($id), 'pg_id')),
            'returnURI' => $_SERVER['HTTP_REFERER'],
            'name' => $_POST['name'] ?? '',
            'symbol' => $_POST['symbol'] ?? '',
            'amount' => $_POST['amount'] ?? '',
            'dueDate' => $_POST['dueDate'] ?? '',
            'validRange' => $_POST['validRange'] ?? '',
            'usePrefix' => $_POST['usePrefix'] ?? '',
            'archive' => $_POST['archive'] ?? '',
            'visible' => $_POST['visible'] ?? '',
        ]);
    }

    protected static function checkData($action, $id): \Form
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
                $active ? ('Už existuje aktivní specifický symbol se symbolem ' . $_POST['symbol'] . ' (' . $active['pc_name'] . ')') : ''
            );
        }
        $f->checkNotEmpty($_POST['name'], 'Zadejte prosím nějaké jméno.');
        $f->checkNumeric($_POST['symbol'], 'Zadejte prosím platný specifický symbol.');
        $f->checkRegexp($_POST['amount'], '/(\*)?([0-9]+)([.,][0-9]+)?/', 'Zadejte prosím platnou očekávanou částku.');

        return $f;
    }
}
