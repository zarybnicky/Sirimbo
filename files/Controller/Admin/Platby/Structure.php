<?php
include_once('files/Controller/Admin/Platby.php');
class Controller_Admin_Platby_Structure extends Controller_Admin_Platby {
    function __construct() {
        Permissions::checkError('platby', P_OWNED);
    }
    function view($id = null) {
        $this->render('files/View/Admin/Platby/StructureOverview.inc', array(
                'data' => $this->getCategories(),
                'orphanGroupSkupina' => $this->getOrphanGroupSkupina(),
                'orphanGroupCategory' => $this->getOrphanGroupCategory(),
                'orphanCategory' => $this->getOrphanCategory()
        ));
    }
    protected function getCategories() {
        $out = array();
        $categories = parent::getCategoryList();
        
        $current_group = 0;
        foreach($categories as $array) {
            $new_data = array();
            if(strpos($array[0], 'group_') !== false) {
                $new_data['name'] = '<span class="big" style="text-decoration:underline;">' . $array[1]['pg_name'] . '</span>';
                $new_data['buttons'] = $this->getEditLink('/admin/platby/structure/group/edit/' . $array[1]['pg_id']) .
                        $this->getRemoveLink('/admin/platby/structure/group/remove/' . $array[1]['pg_id']);
            } else {
                $new_data['name'] = '&nbsp;- ' . $array[1]['pc_name'] . ' (' . $array[1]['pc_symbol'] . ')';
                $new_data['buttons'] = $this->getEditLink('/admin/platby/structure/category/edit/' . $array[1]['pc_id']) . 
                    $this->getRemoveLink('/admin/platby/structure/category/remove/' . $array[1]['pc_id']);
            }
            $out[] = $new_data;
        }
        return $out;
    }
    protected function getOrphanGroupSkupina() {
        $out = DBPlatbyGroup::getWithoutSkupina();
        foreach($out as &$array) {
            $new_data = array(
                    'name' => $array['pg_name'],
                    'buttons' => $this->getEditLink('/admin/platby/structure/group/edit/' . $array['pg_id']) .
                        $this->getRemoveLink('/admin/platby/structure/group/remove/' . $array['pg_id'])
            );
            $array = $new_data;
        }
        return $out;
    }
    protected function getOrphanGroupCategory() {
        $out = DBPlatbyGroup::getWithoutCategory();
        foreach($out as &$array) {
            $new_data = array(
                    'name' => $array['pg_name'],
                    'buttons' => $this->getEditLink('/admin/platby/structure/group/edit/' . $array['pg_id']) .
                        $this->getRemoveLink('/admin/platby/structure/group/remove/' . $array['pg_id'])
            );
            $array = $new_data;
        }
        return $out;
    }
    protected function getOrphanCategory() {
        $out = DBPlatbyCategory::getOrphan();
        foreach($out as &$array) {
            $new_data = array(
                    'name' => $array['pc_name'],
                    'buttons' => $this->getEditLink('/admin/platby/structure/category/edit/' . $array['pc_id']) . 
                        $this->getRemoveLink('/admin/platby/structure/category/remove/' . $array['pc_id'])
            );
            $array = $new_data;
        }
        return $out;
    }
    protected function getEditLink($link) {
        return '<a href="' . $link . '"><img alt="Upravit" src="/images/wrench.png" /></a>';
    }
    protected function getRemoveLink($link) {
        return '<a href="' . $link . '"><img alt="Odstranit" src="/images/cross.png" /></a>';
    }
    protected function getUnlinkGroupButton($id) {
        return
            '<input type="hidden" name="group" value="' . $id . '">' .
            '<button name="action" value="group_remove">' .
                '<img alt="Odstranit spojení" src="/images/unlink.png" />' .
            '</button>';
    }
    protected function getUnlinkCategoryButton($id) {
        return
            '<input type="hidden" name="category" value="' . $id . '">' .
            '<button name="action" value="category_remove">' .
                '<img alt="Odstranit spojení" src="/images/unlink.png" />' .
            '</button>';
    }
    protected function getUnlinkSkupinaButton($id) {
        return
            '<input type="hidden" name="skupina" value="' . $id . '">' .
            '<button name="action" value="skupina_remove">' .
                '<img alt="Odstranit spojení" src="/images/unlink.png" />' .
            '</button>';
    }
    protected function getDateDisplay($from, $to) {
        $from = new Date($from);
        $to = new Date($to);
        return $from->getDate(Date::FORMAT_SIMPLIFIED) . ' - ' . (string) $to->getDate(Date::FORMAT_SIMPLIFIED);
    }
}