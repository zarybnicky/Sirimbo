<?php
namespace TKOlomouc\Controller\Admin\Platby\Structure;

use TKOlomouc\Controller\Admin\Platby\Structure;
use TKOlomouc\Utility\Permissions;
use TKOlomouc\Utility\Request;
use TKOlomouc\Utility\Form;
use TKOlomouc\Model\DBPlatby;
use TKOlomouc\Model\DBPlatbyGroup;
use TKOlomouc\Model\DBPlatbyCategory;
use TKOlomouc\Model\DBPlatbyRaw;
use TKOlomouc\Model\DBPlatbyItem;
use TKOlomouc\View\Helper\Date;

class Category extends Structure
{
    public function __construct()
    {
        Permissions::checkError('platby', P_OWNED);
    }

    public function view($id = null)
    {
        $this->render(
            'src/application/View/Admin/Platby/StructureSymbolOverview.inc',
            array(
                'data' => $this->getCategories(false),
                'archived' => $this->getCategories(true)
            )
        );
    }

    protected function getCategories($archived = false)
    {
        $out = array();

        $categories = DBPlatbyCategory::get($archived);
        foreach ($categories as $array) {
            $new_data = array();
            $new_data['name'] = $array['pc_name'];
            $new_data['symbol'] = $array['pc_symbol'];
            $new_data['amount'] = ($array['pc_amount'] . ($array['pc_use_base'] ? ' * ?' : '')) . ' Kč';
            $new_data['validDate'] = $this->getDateDisplay($array['pc_valid_from'], $array['pc_valid_to']);
            $new_data['buttons'] =
                $this->getEditLink('/admin/platby/structure/category/edit/' . $array['pc_id'])
                . $this->getRemoveLink('/admin/platby/structure/category/remove/' . $array['pc_id']);
            $out[] = $new_data;
        }
        return $out;
    }

    public function add($id = null)
    {
        if (empty($_POST) || is_object($form = $this->checkPost('add'))) {
            if (!empty($_POST)) {
                $this->redirect()->setMessage($form->getMessages());
            }
            $this->displayForm('add');
            return;
        }
        $dueDate = (new Date('dueDate'))->getPost();

        $validRange = (new Date('validRange'))->range()->getPostRange();

        $validFrom = $validRange['from'];
        $validTo = $validRange['to'];

        if (!$validTo->isValid()) {
            $validTo = $validFrom;
        } elseif (strcasecmp((string) $validFrom, (string) $validTo) > 0) {
            $validFrom = $validTo;
        }
        $amount = post('amount');
        $use_base = '0';
        if (strpos($amount, '*') !== false) {
            $use_base = '1';
            $amount = str_replace('*', '', $amount);
        }
        $use_prefix = post('usePrefix') ? '1' : '0';
        $archive = post('archive') ? '1' : '0';

        DBPlatbyCategory::insert(
            post('name'),
            post('symbol'),
            $amount,
            (string) $dueDate,
            (string) $validFrom,
            (string) $validTo,
            $use_base,
            $use_prefix,
            $archive
        );
        $insertId = DBPlatbyCategory::getInsertId();
        if (get('group') && ($data = DBPlatbyGroup::getSingle(get('group')))) {
            DBPlatbyGroup::addChild(get('group'), $insertId);
            $skupiny = DBPlatbyGroup::getSingleWithSkupiny(get('group'));
            $conflicts = array();

            foreach ($skupiny as $array) {
                $conflicts = array_merge($conflicts, DBPlatby::checkConflicts($array['s_id']));
            }
            if (!empty($conflicts)) {
                DBPlatbyGroup::removeChild(get('category'), $insertId);
                $this->redirect(
                    '/admin/platby/structure/group/edit/' . get('group'),
                    'Specifický symbol byl přidán, ale nebyl přiřazen - takové přiřazení není platné.'
                );
            }
            $this->redirect(
                '/admin/platby/structure/group/edit/' . get('group'),
                'Specifický symbol úspěšně přidán a přiřazen'
            );
        }
        $this->redirect(
            post('referer') ? post('referer') : '/admin/platby/structure',
            'Specifický symbol úspěšně přidán'
        );
    }

    public function edit($id = null)
    {
        if (!$id || !($data = DBPlatbyCategory::getSingle($id))) {
            $this->redirect(
                post('referer') ? post('referer') : '/admin/platby/structure',
                'Kategorie s takovým ID neexistuje'
            );
        }

        if (post('action') == 'group') {
            if (!($data = DBPlatbyGroup::getSingle(post('group')))) {
                $this->redirect(
                    '/admin/platby/structure/category/edit/' . $id,
                    'Kategorie s takovým ID neexistuje.'
                );
            }

            DBPlatbyGroup::addChild(post('group'), $id);
            $skupiny = DBPlatbyGroup::getSingleWithSkupiny(post('group'));
            $conflicts = array();

            foreach ($skupiny as $array) {
                $conflicts = array_merge($conflicts, DBPlatby::checkConflicts($array['s_id']));
            }
            if (!empty($conflicts)) {
                DBPlatbyGroup::removeChild(post('group'), $id);
                $this->redirect(
                    '/admin/platby/structure/category/edit/' . $id,
                    'Takové přiřazení není platné - způsobilo by, '
                    . 'že jeden specifický symbol by byl v jedné skupině dvakrát.'
                );
            }
            $this->redirect(
                '/admin/platby/structure/category/edit/' . $id,
                'Kategorie byla úspěšně přiřazena.'
            );
        } elseif (post('action') == 'group_remove') {
            if (!($data = DBPlatbyGroup::getSingle(post('group')))) {
                $this->redirect(
                    '/admin/platby/structure/category/edit/' . $id,
                    'Kategorie s takovým ID neexistuje.'
                );
            }
            DBPlatbyGroup::removeChild(post('group'), $id);
            $this->redirect(
                '/admin/platby/structure/category/edit/' . $id,
                'Spojení s kategorií bylo úspěšně odstraněno.'
            );
        }

        if (empty($_POST) || is_object($form = $this->checkPost('edit'))) {
            if (!empty($_POST)) {
                $this->redirect()->setMessage($form->getMessages());
            } else {
                if ($data['pc_use_base']) {
                    $data['pc_amount'] = '*' . $data['pc_amount'];
                }
                post('name', $data['pc_name']);
                post('symbol', $data['pc_symbol']);
                post('amount', $data['pc_amount']);
                post('dueDate', $data['pc_date_due']);
                post('validRange', $data['pc_valid_from'] . ' - ' . $data['pc_valid_to']);
                post('usePrefix', $data['pc_use_prefix']);
                post('archive', $data['pc_archive']);
            }
            $this->displayForm('edit');
            return;
        }
        $dueDate = (new Date('dueDate'))->getPost();

        $validRange = (new Date('validRange'))->range()->getPostRange();
        $validFrom = $validRange['from'];
        $validTo = $validRange['to'];

        if (!$validTo->isValid()) {
            $validTo = $validFrom;
        } elseif (strcasecmp((string) $validFrom, (string) $validTo) > 0) {
            $validFrom = $validTo;
        }
        $amount = post('amount');
        $use_base = '0';
        if (strpos($amount, '*') !== false) {
            $use_base = '1';
            $amount = str_replace('*', '', $amount);
        }
        $use_prefix = post('usePrefix') ? '1' : '0';
        $archive = post('archive') ? '1' : '0';

        DBPlatbyCategory::update(
            $id,
            post('name'),
            post('symbol'),
            $amount,
            (string) $dueDate,
            (string) $validFrom,
            (string) $validTo,
            $use_base,
            $use_prefix,
            $archive
        );
        if (get('group')) {
            $this->redirect(
                '/admin/platby/structure/group/edit/' . get('group'),
                'Specifický symbol úspěšně upraven'
            );
        }
        $this->redirect(
            post('referer') ? post('referer') : '/admin/platby/structure',
            'Specifický symbol úspěšně upraven'
        );
    }

    public function remove($id = null)
    {
        if (!$id || !($data = DBPlatbyCategory::getSingle($id))) {
            $this->redirect(
                post('referer') ? post('referer') : '/admin/platby/structure',
                'Specifický symbol s takovým ID neexistuje'
            );
        }

        if (post('action') == 'unlink') {
            $linked = $this->getLinkedObjects($id);

            $groupCount = 0;
            foreach ($linked['groups'] as $data) {
                DBPlatbyGroup::removeChild($data['pg_id'], $id);
                ++$groupCount;
            }
            unset($data);
            $itemCount = 0;
            foreach ($linked['items'] as $data) {
                $raw = DBPlatbyRaw::getSingle($data['pi_id_raw']);
                DBPlatbyRaw::update($raw['pr_id'], $raw['pr_raw'], $raw['pr_hash'], '0', '0');
                DBPlatbyItem::remove($data['pi_id']);
                ++$itemCount;
            }
            $this->redirect(
                '/admin/platby/structure/category/remove/' . $id,
                'Spojení s ' . $groupCount . ' kategoriemi a s ' . $itemCount . ' platbami bylo odstraněno'
            );
            return;
        } elseif (post('action') == 'archive') {
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
                '1'
            );
            $this->redirect(
                '/admin/platby/structure/category',
                'Specifický symbol "' . $data['pc_symbol'] . '" byl archivován'
            );
            return;
        }
        if (((empty($_POST) || post('action') == 'confirm')
            && ($linked = $this->getLinkedObjects($id)))
            || empty($_POST)
        ) {
            if (isset($linked) && $linked) {
                $this->redirect()->setMessage(
                    'Nemůžu odstranit specifický symbol s připojenými kategoriemi nebo položkami! '
                    . '<form action="" method="post">'
                    . (!$data['pc_archive']
                      ? '<button type="submit" name="action" value="archive">Archivovat?</button> nebo '
                      : '')
                    . '<button type="submit" name="action" value="unlink">'
                    . 'Odstranit všechna spojení se skupinami a kategoriemi'
                    . ' a přesunout ovlivněné platby do nezařazených?</button>'
                    . '</form>'
                );
            }
            $this->render(
                'src/application/View/Admin/Platby/StructureSymbolRemove.inc',
                array(
                    'id' => $id,
                    'name' => $data['pc_name']
                )
            );
            return;
        }
        DBPlatbyCategory::delete($id);
        $this->redirect(
            post('referer') ? post('referer') : '/admin/platby/structure',
            'Specifický symbol byl odebrán'
        );
    }

    private function getLinkedObjects($id)
    {
        $group = DBPlatbyCategory::getSingleWithGroups($id);
        $items = DBPlatbyItem::get(true, array('pc_id' => $id));

        if (empty($group) && empty($items)) {
            return array();
        } else {
            return array('groups' => $group, 'items' => $items);
        }
    }

    private function displayForm($action)
    {
        $id = Request::getID() ? Request::getID() : 0;

        $groups = DBPlatbyCategory::getSingleWithGroups($id);
        foreach ($groups as &$array) {
            $new_data = array(
                'buttons' => '<form action="" method="post">'
                    . $this->getUnlinkGroupButton($array['pg_id'])
                    . $this->getEditLink('/admin/platby/structure/group/edit/' . $array['pg_id'])
                    . $this->getRemoveLink('/admin/platby/structure/group/remove/' . $array['pg_id'])
                    . '</form>',
                'type' => ($array['pg_type'] == '1' ? 'Členské příspěvky' : 'Běžné platby'),
                'name' => $array['pg_name'],
                'base' => $array['pg_base']
            );
            $array = $new_data;
        }unset($array);

        $groupNotInCategory = DBPlatbyGroup::getNotInCategory($id);
        $groupSelect = array();
        foreach ($groupNotInCategory as $array) {
            $groupSelect[$array['pg_id']] = $array['pg_name'];
        }unset($array);

        $this->render(
            'src/application/View/Admin/Platby/StructureSymbolForm.inc',
            array(
                'id' => $id,
                'action' => $action,
                'groups' => $groups,
                'groupSelect' => $groupSelect
            )
        );
    }

    protected function checkPost($action)
    {
        $form = new Form();

        $dueDate = (new Date('dueDate'))->getPost();
        if ($dueDate->getYear() == '0000') {
            $dueDate = str_replace('0000', '2000', (string) $dueDate);
        }
        $form->checkDate($dueDate, 'Datum splatnosti není platné.');

        $validRange = (new Date('validRange'))->range()->getPostRange();
        if ($validRange['from']->getYear() == '0000') {
            $form->checkDate(
                str_replace('0000', '2000', (string) $validRange['from']),
                'Datum platnosti není platné'
            );
            if ($validRange['to']->isValid() && $validRange['to']->getYear() == '0000') {
                $form->checkDate(
                    str_replace('0000', '2000', (string) $validRange['to']),
                    'Datum platnosti (část \'do\') není platné'
                );
            } else {
                $form->checkDate(
                    (string) $validRange['from'],
                    'Datum platnosti (část \'do\') není platné'
                );
            }
        } else {
            $form->checkDate(
                (string) $validRange['from'],
                'Datum platnosti není platné'
            );
            if ($validRange['to']->isValid()) {
                $form->checkDate(
                    (string) $validRange['from'],
                    'Datum platnosti (část \'do\') není platné'
                );
            }
        }
        if (!post('archive')) {
            $form->checkBool(
                !($active = DBPlatbyCategory::checkActiveSymbol(post('symbol')))
                    || ($action == 'edit' ? $active['pc_id'] == Request::getID() : false),
                $active
                    ? ('Už existuje aktivní specifický symbol se symbolem '
                        . post('symbol') . ' (' . $active['pc_name'] . ')')
                    : '',
                ''
            );
        }
        $form->checkNotEmpty(post('name'), 'Zadejte prosím nějaké jméno.');
        $form->checkNumeric(post('symbol'), 'Zadejte prosím platný specifický symbol.');
        $form->checkRegexp(
            post('amount'),
            '/(\*)?([0-9]+)([.,][0-9]+)?/',
            'Zadejte prosím platnou očekávanou částku.'
        );

        return $form->isValid() ? true : $form;
    }
}
