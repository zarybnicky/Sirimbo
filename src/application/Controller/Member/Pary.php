<?php
namespace TKOlomouc\Controller\Member;

use TKOlomouc\Controller\Member;
use TKOlomouc\Utility\Permissions;
use TKOlomouc\Model\DBPary;

class Pary extends Member
{
    function __construct() {
        Permissions::checkError('pary', P_VIEW);
    }
    function view($id = null) {
        $pary = DBPary::getActiveParyByHodnoceni();
        if (empty($pary)) {
            $this->render(
                'src/application/View/Empty.inc',
                array(
                    'nadpis' => 'Žebříček párů',
                    'notice' => 'Žádné páry nejsou v databázi'
                )
            );
            return;
        }
        foreach ($pary as &$item) {
            $new_data = array(
                'id' => $item['p_id'],
                'partnerName' => $item['guy_name'] . ' ' . $item['guy_surname'],
                'partnerkaName' => $item['gal_name'] . ' ' . $item['gal_surname'],
                'latina' => $item['p_stt_trida'] . ' ' . $item['p_stt_body'] . 'F' . $item['p_stt_finale'],
                'standart' => $item['p_lat_trida'] . ' ' . $item['p_lat_body'] . 'F' . $item['p_lat_finale'],
                'hodnoceni' => $item['p_hodnoceni']
            );
            $item = $new_data;
        }
        $this->render(
            'src/application/View/Member/Pary/Overview.inc',
            array(
                'data' => $pary
            )
        );
    }
}
?>