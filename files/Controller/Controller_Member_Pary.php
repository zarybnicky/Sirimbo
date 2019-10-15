<?php
class Controller_Member_Pary extends Controller_Member
{
    public function __construct()
    {
        parent::__construct();
        Permissions::checkError('pary', P_VIEW);
    }

    public function view($request)
    {
        $data = DBPary::getActiveParyByHodnoceni();
        if (empty($data)) {
            $this->render('files/View/Empty.inc', [
                'header' => 'Žebříček párů',
                'notice' => 'Žádné páry nejsou v databázi'
            ]);
            return;
        }
        $data = array_map(
            function ($item) {
                return [
                    'id' => $item['p_id'],
                    'partnerName' => $item['guy_name'] . ' ' . $item['guy_surname'],
                    'partnerkaName' => $item['gal_name'] . ' ' . $item['gal_surname'],
                    'latina' => $item['p_stt_trida'] . ' ' . $item['p_stt_body'] . 'F' . $item['p_stt_finale'],
                    'standard' => $item['p_lat_trida'] . ' ' . $item['p_lat_body'] . 'F' . $item['p_lat_finale'],
                    'hodnoceni' => $item['p_hodnoceni']
                ];
            },
            $data
        );

        $this->render('files/View/Member/Pary/Overview.inc', [
            'header' => 'Žebříček párů',
            'data' => $data
        ]);
    }
}
