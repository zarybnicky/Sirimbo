<?php
class Controller_Admin_Platby_Structure extends Controller_Admin_Platby
{
    public function __construct()
    {
        parent::__construct();
        Permissions::checkError('platby', P_OWNED);
    }

    public function view($request)
    {
        $this->render('files/View/Admin/Platby/StructureOverview.inc', [
            'header' => 'Správa plateb',
            'subheader' => 'Struktura plateb',
            'data' => $this->getCategories(),
            'orphanGroupSkupina' => $this->getOrphanGroupSkupina(),
            'orphanGroupCategory' => $this->getOrphanGroupCategory(),
            'orphanCategory' => $this->getOrphanCategory(),
            'uri' => $request->getLiteralURI()
        ]);
    }

    protected function getCategories()
    {
        return array_map(
            function ($item) {
                if (strpos($item[0], 'group_') !== false) {
                    $prefix = '/admin/platby/structure/group';
                    return [
                        'name' => new Tag(
                            'span',
                            ['class' => 'big', 'style' => 'text-decoration:underline'],
                            $item[1]['pg_name']
                        ),
                        'buttons' => (
                            $this->editLink($prefix . '/edit/' . $item[1]['pg_id']) .
                            $this->removeLink($prefix . '/remove/' . $item[1]['pg_id'])
                        )
                    ];
                } else {
                    $prefix = '/admin/platby/structure/category';
                    return [
                        'name' => '&nbsp;- ' . $item[1]['pc_name'] . ' (' . $item[1]['pc_symbol'] . ')',
                        'buttons' => (
                            $this->editLink($prefix . '/edit/' . $item[1]['pc_id'])
                            . $this->removeLink($prefix . '/remove/' . $item[1]['pc_id'])
                        )
                    ];
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
                return [
                    'name' => $item['pg_name'],
                    'buttons' => (
                        $this->editLink($prefix .'/edit/' . $item['pg_id'])
                        . $this->removeLink($prefix . '/remove/' . $item['pg_id'])
                    )
                ];
            },
            DBPlatbyGroup::getWithoutSkupina()
        );
    }

    protected function getOrphanGroupCategory()
    {
        $prefix = '/admin/platby/structure/group';
        return array_map(
            function ($item) use ($prefix) {
                return [
                    'name' => $item['pg_name'],
                    'buttons' => (
                        $this->editLink($prefix .'/edit/' . $item['pg_id'])
                        . $this->removeLink($prefix . '/remove/' . $item['pg_id'])
                    )
                ];
            },
            DBPlatbyGroup::getWithoutCategory()
        );
    }
    protected function getOrphanCategory()
    {
        $prefix = '/admin/platby/structure/category';
        return array_map(
            function ($item) use ($prefix) {
                return [
                    'name' => $item['pc_name'],
                    'buttons' => (
                        $this->editLink($prefix . '/edit/' . $item['pc_id'])
                        . $this->removeLink($prefix . '/remove/' . $item['pc_id'])
                    )
                ];
            },
            DBPlatbyCategory::getOrphan()
        );
    }

    protected function getDuplicateCategoryButton($id)
    {
        return $this->submit('<img title="Duplikovat" alt="Duplikovat" src="/style/icon-files-o.png" />')
            ->data('category_duplicate', $id)->cls('a');
    }

    protected function getDateDisplay($from, $to)
    {
        $from = new Date($from);
        $to = new Date($to);
        return $from->getDate(Date::FORMAT_SIMPLIFIED)
            . ' - '
            . $to->getDate(Date::FORMAT_SIMPLIFIED);
    }
}
