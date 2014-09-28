<?php
require_once 'files/Controller/Admin/Rozpis.php';
class Controller_Admin_Rozpis_Detail extends Controller_Admin_Rozpis
{
    function __construct() {
        Permissions::checkError('rozpis', P_OWNED);
    }
    function view($id = null) {
        if (!$id || !($data = DBRozpis::getSingleRozpis($id))) {
            $this->redirect(
                '/admin/rozpis',
                'Rozpis s takovým ID neexistuje'
            );
        }

        Permissions::checkError('rozpis', P_OWNED, $data['r_trener']);

        $items = DBRozpis::getRozpisItem($id);
        $users = DBPary::getPartners();

        if (empty($_POST)) {
            $this->render(
                'files/Admin/RozpisDetail/Display.inc',
                array(
                    'data' => $data,
                    'users' => $users,
                    'items' => $items
                )
            );
            return;
        }
        if (post('generate') == 'overlap') {
            $end = "00:00";
            foreach ($items as $item) {
                $l_start = formatTime($item['ri_od'], 1);
                $l_end = formatTime($item['ri_do'], 1);
                $length = timeSubstract($l_end, $l_start);

                if (strcmp($end, $l_start) > 0) {
                    $l_start = $end;
                    $l_end = timeAdd($l_start, $length);
                }
                if (strcmp($l_start, $l_end) > 0)
                    $l_end = timeAdd($l_start, $length);

                DBRozpis::editRozpisItem(
                    $item['ri_id'], $item['ri_partner'],
                    formatTime($l_start, 0), formatTime($l_end, 0), $item['ri_lock']
                );

                $end = $l_end;
            }
            $items = DBRozpis::getRozpisItem($id);
        } elseif (post('generate') == 'gen_add'
            && is_numeric(post('gen_add_hod'))
            && is_numeric(post('gen_add_len'))
            && preg_match('/^[0-9]{2}:[0-9]{2}$/', post('gen_add_od'))
        ) {
            $od = post('gen_add_od');
            $len = floor(post('gen_add_len') / 60) . ':'
                   . (int)(post('gen_add_len') % 60);
            $do = timeAdd($od, $len);

            for($i = 0; $i < post('gen_add_hod'); $i++) {
                DBRozpis::addRozpisItem(
                    $id, '0', formatTime($od, 0),
                    formatTime($do, 0), '0'
                );
                $od = $do;
                $do = timeAdd($od, $len);
            }
            $items = DBRozpis::getRozpisItem($id);
        } else {
            if (post('remove') > 0) {
                DBRozpis::removeRozpisItem(post('remove'));
                $items = DBRozpis::getRozpisItem($id);
            }
            foreach ($items as $item) {
                if (!(post($item['ri_id'] . '-partner') == $item['ri_partner']
                    || (post($item['ri_id'] . '-partner') == 'none' && $item['ri_partner'] == '0'))
                    || post($item['ri_id'] . '-od') != formatTime($item['ri_od'], 1)
                    || post($item['ri_id'] . '-do') != formatTime($item['ri_do'], 1)
                    || (bool) post($item['ri_id'] . '-lock') != (bool) $item['ri_lock']) {

                    $partner = (post($item['ri_id'] . '-partner') == 'none')
                               ? '0'
                               : post($item['ri_id'] . '-partner');
                    $od = post($item['ri_id'] . '-od');
                    $do = post($item['ri_id'] . '-do');
                    $lock = post($item['ri_id'] . '-lock') ? 1 : 0;

                    if (preg_match('/^[0-9]{2}:[0-9]{2}$/', $od)
                        && preg_match('/^[0-9]{2}:[0-9]{2}$/', $do)) {
                        DBRozpis::editRozpisItem($item['ri_id'], $partner,
                            formatTime($od, 0), formatTime($do, 0), $lock);
                    }
                }
            }
            $items = DBRozpis::getRozpisItem($id);

            if (is_numeric(post('add_partner')) || post('add_partner') == 'none') {
                $partner = (post('add_partner') == 'none') ? "0" : post('add_partner');
                $od = post('add_od');
                $do = post('add_do');
                $lock = post('add_lock');

                if (preg_match('/^[012]*[0-9]{1}:[0-9]{2}$/', $od)
                    && preg_match('/^[012]*[0-9]{1}:[0-9]{2}$/', $do)) {
                    DBRozpis::addRozpisItem($id, $partner, formatTime($od, 0), formatTime($do, 0), $lock);
                    post('add_partner', null);
                    $items = DBRozpis::getRozpisItem($id);
                }
            }
        }
        $this->render(
            'files/Admin/RozpisDetail/Display.inc',
            array(
                'data' => $data,
                'users' => $users,
                'items' => $items
            )
        );
    }
}
