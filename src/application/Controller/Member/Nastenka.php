<?php
namespace TKOlomouc\Controller\Member;

use TKOlomouc\Controller\Member;
use TKOlomouc\Model\Paging\Pager;
use TKOlomouc\Model\Paging\PagerAdapterDb;
use TKOlomouc\Model\DBNastenka;
use TKOlomouc\Utility\Permissions;

class Nastenka extends Member
{
    function __construct() {
        Permissions::checkError('nastenka', P_VIEW);
    }
    function view($id = null) {
        $pager = new Pager(new PagerAdapterDb('DBNastenka'));
        $pager->setCurrentPageField('p');
        $pager->setItemsPerPageField('c');
        $pager->setDefaultItemsPerPage(10);
        $pager->setPageRange(5);
        $data = $pager->getItems();

        if (empty($data)) {
            $this->render(
                'src/application/View/Empty.inc',
                array(
                    'nadpis' => 'Upozornění',
                    'notice' => 'Žádná upozornění nejsou k dispozici'
                )
            );
            return;
        }
        foreach ($data as &$item) {
            $skupiny = DBNastenka::getNastenkaSkupiny($item['up_id']);
            foreach ($skupiny as &$skupina) {
                $new_data = getColorBox($skupina['ups_color'], $skupina['ups_popis']);
                $skupina = $new_data;
            }
            $new_data = array(
                'id' => $item['up_id'],
                'nadpis' => $item['up_nadpis'],
                'canEdit' => Permissions::check('nastenka', P_OWNED, $item['up_kdo']),
                'skupinyBoxes' => $skupiny,
                'addedBy' => $item['u_jmeno'] . ' ' . $item['u_prijmeni'],
                'addedTimestamp' => formatTimestamp($item['up_timestamp_add']),
                'text' => stripslashes($item['up_text'])
            );
            $item = $new_data;
        }
        $this->render(
            'src/application/View/Member/Nastenka.inc',
            array(
                'data' => $data,
                'navigation' => $pager->getNavigation()
            )
        );
    }
}
?>