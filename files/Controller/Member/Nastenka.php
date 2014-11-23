<?php
require_once 'files/Controller/Member.php';
class Controller_Member_Nastenka extends Controller_Member
{
    public function __construct() {
        Permissions::checkError('nastenka', P_VIEW);
    }
    public function view($request) {
        $pager = new Paging(new PagingAdapterDBSelect('DBNastenka'));
        $pager->setCurrentPageField('p');
        $pager->setItemsPerPageField('c');
        $pager->setDefaultItemsPerPage(10);
        $pager->setPageRange(5);
        $data = $pager->getItems();

        if (empty($data)) {
            $this->render(
                'files/View/Empty.inc',
                array(
                    'nadpis' => 'Upozornění',
                    'notice' => 'Žádná upozornění nejsou k dispozici'
                )
            );
            return;
        }
        foreach ($data as &$item) {
            $skupiny = array_map(
                function ($skupina) {
                    return (string) $this->colorbox(
                        $skupina['ups_color'],
                        $skupina['ups_popis']
                    );
                },
                DBNastenka::getNastenkaSkupiny($item['up_id'])
            );
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
            'files/View/Member/Nastenka.inc',
            array(
                'data' => $data,
                'navigation' => $pager->getNavigation()
            )
        );
    }
}
?>