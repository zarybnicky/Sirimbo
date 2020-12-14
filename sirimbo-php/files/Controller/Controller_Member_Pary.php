<?php
class Controller_Member_Pary
{
    public function view($request)
    {
        Permissions::checkError('pary', P_VIEW);
        $data = DBPary::getActiveParyByHodnoceni();
        if (empty($data)) {
            return new \RenderHelper('files/View/Empty.inc', [
                'header' => 'Žebříček párů',
                'notice' => 'Žádné páry nejsou v databázi'
            ]);
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

        new \RenderHelper('files/View/Member/Pary/Overview.inc', [
            'header' => 'Žebříček párů',
            'data' => $data
        ]);
    }
}
