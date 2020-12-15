<?php
class Controller_Admin_Platby_Structure extends Controller_Admin_Platby
{
    public function view()
    {
        \Permissions::checkError('platby', P_OWNED);
        new \RenderHelper('files/View/Admin/Platby/StructureOverview.inc', [
            'header' => 'Správa plateb',
            'subheader' => 'Struktura plateb',
            'data' => static::getCategories(),
            'orphanGroupSkupina' => static::getOrphanGroupSkupina(),
            'orphanGroupCategory' => static::getOrphanGroupCategory(),
            'orphanCategory' => static::getOrphanCategory(),
            'uri' => trim(explode('?', $_SERVER['REQUEST_URI'])[0], '/')
        ]);
    }

    protected static function getCategories()
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
                            new \EditLinkHelper($prefix . '/edit/' . $item[1]['pg_id']) .
                            new \RemoveLinkHelper($prefix . '/remove/' . $item[1]['pg_id'])
                        )
                    ];
                } else {
                    $prefix = '/admin/platby/structure/category';
                    return [
                        'name' => '&nbsp;- ' . $item[1]['pc_name'] . ' (' . $item[1]['pc_symbol'] . ')',
                        'buttons' => (
                            new \EditLinkHelper($prefix . '/edit/' . $item[1]['pc_id']) .
                            new \RemoveLinkHelper($prefix . '/remove/' . $item[1]['pc_id'])
                        )
                    ];
                }
            },
            parent::getCategoryList()
        );
    }

    protected static function getOrphanGroupSkupina()
    {
        $prefix = '/admin/platby/structure/group';
        return array_map(
            fn($item) => [
                'name' => $item['pg_name'],
                'buttons' => (
                    new \EditLinkHelper($prefix .'/edit/' . $item['pg_id']) .
                    new \RemoveLinkHelper($prefix . '/remove/' . $item['pg_id'])
                )
            ],
            \DBPlatbyGroup::getWithoutSkupina()
        );
    }

    protected static function getOrphanGroupCategory()
    {
        $prefix = '/admin/platby/structure/group';
        return array_map(
            fn($item) => [
                'name' => $item['pg_name'],
                'buttons' => (
                    new EditLinkHelper($prefix .'/edit/' . $item['pg_id']) .
                    new RemoveLinkHelper($prefix . '/remove/' . $item['pg_id'])
                ),
            ],
            \DBPlatbyGroup::getWithoutCategory()
        );
    }
    protected static function getOrphanCategory()
    {
        $prefix = '/admin/platby/structure/category';
        return array_map(
            fn($item) => [
                'name' => $item['pc_name'],
                'buttons' => (
                    new EditLinkHelper($prefix . '/edit/' . $item['pc_id'])
                    . new RemoveLinkHelper($prefix . '/remove/' . $item['pc_id'])
                )
            ],
            \DBPlatbyCategory::getOrphan()
        );
    }
}
