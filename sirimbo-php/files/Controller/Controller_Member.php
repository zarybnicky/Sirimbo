<?php
class Controller_Member extends Controller_Abstract
{
    public function view($request)
    {
        Permissions::checkError('nastenka', P_VIEW);
        $pager = new Paging(new DBNastenka());
        $pager->setCurrentPage($request->get('p'));
        $pager->setItemsPerPage($request->get('c'));
        $pager->setDefaultItemsPerPage(10);
        $data = $pager->getItems();

        if (empty($data)) {
            return new \RenderHelper('files/View/Empty.inc', [
                'header' => 'Upozornění',
                'notice' => 'Žádná upozornění nejsou k dispozici'
            ]);
        }

        $data = array_map(
            function ($item) {
                $skupiny = array_map(
                    fn($skupina) => new ColorboxHelper($skupina['ups_color'], $skupina['ups_popis']),
                    DBNastenka::getNastenkaSkupiny($item['up_id'])
                );
                return [
                    'id' => $item['up_id'],
                    'nadpis' => $item['up_nadpis'],
                    'canEdit' => Permissions::check('nastenka', P_OWNED, $item['up_kdo']),
                    'skupinyBoxes' => implode('', $skupiny),
                    'addedBy' => $item['u_jmeno'] . ' ' . $item['u_prijmeni'],
                    'addedTimestamp' => formatTimestamp($item['up_timestamp_add']),
                    'text' => stripslashes($item['up_text'])
                ];
            },
            $data
        );

        new \RenderHelper('files/View/Member/Nastenka.inc', [
            'header' => 'Upozornění',
            'data' => $data,
            'navigation' => $pager->getNavigation($request->get())
        ]);
    }
}
