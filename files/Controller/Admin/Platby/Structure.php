<?php
require_once 'files/Controller/Admin/Platby.php';
class Controller_Admin_Platby_Structure extends Controller_Admin_Platby
{
    public function __construct()
    {
        Permissions::checkError('platby', P_OWNED);
    }

    public function view($request)
    {
        $this->render(
            'files/View/Admin/Platby/StructureOverview.inc',
            array(
                'data' => $this->getCategories(),
                'orphanGroupSkupina' => $this->getOrphanGroupSkupina(),
                'orphanGroupCategory' => $this->getOrphanGroupCategory(),
                'orphanCategory' => $this->getOrphanCategory(),
                'uri' => $request->getLiteralURI()
            )
        );
    }

    protected function getCategories()
    {
        return array_map(
            function ($item) {
                if (strpos($item[0], 'group_') !== false) {
                    $prefix = '/admin/platby/structure/group';
                    return array(
                        'name' => '<span class="big" style="text-decoration:underline;">' . $item[1]['pg_name'] . '</span>',
                        'buttons' => (
                            $this->getEditLink($prefix . '/edit/' . $item[1]['pg_id']) .
                            $this->getRemoveLink($prefix . '/remove/' . $item[1]['pg_id'])
                        )
                    );
                } else {
                    $prefix = '/admin/platby/structure/category';
                    return array(
                        'name' => '&nbsp;- ' . $item[1]['pc_name'] . ' (' . $item[1]['pc_symbol'] . ')',
                        'buttons' => (
                            $this->getEditLink($prefix . '/edit/' . $item[1]['pc_id'])
                            . $this->getRemoveLink($prefix . '/remove/' . $item[1]['pc_id'])
                        )
                    );
                }
            },
            parent::getCategoryList()
        );
    }

    protected function getOrphanGroupSkupina()
    {
        $prefix = '/admin/platby/structure/group';
        return array_map(
            function ($item) use ($prefix) {
                return array(
                    'name' => $item['pg_name'],
                    'buttons' => (
                        $this->getEditLink($prefix .'/edit/' . $item['pg_id'])
                        . $this->getRemoveLink($prefix . '/remove/' . $item['pg_id'])
                    )
                );
            },
            DBPlatbyGroup::getWithoutSkupina()
        );
    }

    protected function getOrphanGroupCategory()
    {
        $prefix = '/admin/platby/structure/group';
        return array_map(
            function ($item) use ($prefix) {
                return array(
                    'name' => $item['pg_name'],
                    'buttons' => (
                        $this->getEditLink($prefix .'/edit/' . $item['pg_id'])
                        . $this->getRemoveLink($prefix . '/remove/' . $item['pg_id'])
                    )
                );
            },
            DBPlatbyGroup::getWithoutCategory()
        );
    }
    protected function getOrphanCategory()
    {
        $prefix = '/admin/platby/structure/category';
        return array_map(
            function ($item) use ($prefix) {
                return array(
                    'name' => $item['pc_name'],
                    'buttons' => (
                        $this->getEditLink($prefix . '/edit/' . $item['pc_id'])
                        . $this->getRemoveLink($prefix . '/remove/' . $item['pc_id'])
                    )
                );
            },
            DBPlatbyCategory::getOrphan()
        );
    }

    protected function getEditLink($link)
    {
        return '<a href="' . $link . '"><img alt="Upravit" src="/images/wrench.png" /></a>';
    }

    protected function getRemoveLink($link)
    {
        return '<a href="' . $link . '"><img alt="Odstranit" src="/images/cross.png" /></a>';
    }

    protected function getUnlinkGroupButton($id)
    {
        return $this->hidden('group', $id)
             . $this->submit('<img alt="Odstranit spojení" src="/images/unlink.png" />')
                    ->data('action', 'group_remove');
    }

    protected function getUnlinkCategoryButton($id)
    {
        return $this->hidden('category', $id)
             . $this->submit('<img alt="Odstranit spojení" src="/images/unlink.png" />')
                    ->data('action', 'category_remove');
    }

    protected function getUnlinkSkupinaButton($id)
    {
        return $this->hidden('skupina', $id)
             . $this->submit('<img alt="Odstranit spojení" src="/images/unlink.png" />')
                    ->data('action', 'skupina_remove');
    }

    protected function getDateDisplay($from, $to) {
        $from = new Date($from);
        $to = new Date($to);
        return $from->getDate(Date::FORMAT_SIMPLIFIED)
            . ' - '
            . $to->getDate(Date::FORMAT_SIMPLIFIED);
    }
}