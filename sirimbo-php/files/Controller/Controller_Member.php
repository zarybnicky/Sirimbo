<?php
class Controller_Member extends Controller_Abstract
{
    public function __construct($request)
    {
        parent::__construct($request);
        Permissions::checkError('nastenka', P_VIEW);
    }

    public function view($request)
    {
        $pager = new Paging(new DBNastenka());
        $pager->setCurrentPage($request->get('p'));
        $pager->setItemsPerPage($request->get('c'));
        $pager->setDefaultItemsPerPage(10);
        $data = $pager->getItems();

        if (empty($data)) {
            $this->render('files/View/Empty.inc', [
                'header' => 'Upozornění',
                'notice' => 'Žádná upozornění nejsou k dispozici'
            ]);
            return;
        }

        $data = array_map(
            function ($item) {
                $skupiny = array_map(
                    fn($skupina) => new Colorbox($skupina['ups_color'], $skupina['ups_popis']),
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

        $this->render('files/View/Member/Nastenka.inc', [
            'header' => 'Upozornění',
            'data' => $data,
            'navigation' => $pager->getNavigation($request->get())
        ]);
    }
}
