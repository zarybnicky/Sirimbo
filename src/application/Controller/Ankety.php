<?php
namespace TKOlomouc\Controller;

use TKOlomouc\Utility\Permissions;
use TKOlomouc\Model\DBAnkety;

class Ankety extends ControllerAbstract
{
    function view($id = null) {
        if (post('id')) {
            $currentIP = getIP();
            if (DBAnkety::isUniqueIP(post('id'), $currentIP)) {
                DBAnkety::vote(post('id'), post('choice'), $currentIP);
                notice('Váš hlas byl uložen');
            } else {
                notice('Z vaší IP adresy už někdo hlasoval.');
            }
        }
        if (array() === ($data = DBAnkety::getAnketyWithItems(true))) {
            $this->render('src/application/View/Empty.inc', array(
                    'nadpis' => $nadpis,
                    'notice' => 'Žádné ankety nejsou k dispozici.'
            ));
            return;
        }
        foreach ($data as &$row) {
            $sum = 0;
            foreach ($row['items'] as $item) {
                $sum += $item['aki_pocet'];
            }
            if ($sum == 0) {
                $sum = 1;
            }

            $new_row = array();
            foreach ($row['items'] as $item) {
                $new_row['items'][] = array(
                    'text' => $item['aki_text'],
                    'pocet' => $item['aki_pocet'],
                    'width' => $item['aki_pocet'] / $sum * 100,
                    'color' => dechex(rand(40, 220)) . dechex(rand(40, 220)) . dechex(rand(40, 220))
                );
            }
            $new_row['id'] = $row['ak_id'];
            $new_row['text'] = $row['ak_text'];
            $new_row['canEdit'] = Permissions::check('ankety', P_OWNED, $row['ak_kdo']);

            $row = $new_row;
        }
        echo $this->render(
            'src/application/View/Main/Ankety/Overview.inc',
            array(
                'data' => $data
            )
        );
    }
    function sidebar() { }
}
?>