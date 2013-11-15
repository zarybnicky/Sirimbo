<?php
namespace TKOlomouc\Controller\Admin\Rozpis;

use TKOlomouc\Controller\Admin\Rozpis;
use TKOlomouc\Utility\Permissions;
use TKOlomouc\Model\DBRozpis;
use TKOlomouc\Model\DBPary;
use TKOlomouc\Model\DBNabidka;

class Detail extends Rozpis
{
    function __construct()
    {
        Permissions::checkError('rozpis', P_OWNED);
    }

    function view($id = null)
    {
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
            $this->displayForm($data, $users, $items);
            return;
        }

        if (post('generate') == 'overlap') {
            $this->processOverlap($items);

            $items = DBRozpis::getRozpisItem($id);
        }

        if (post('generate') == 'gen_add') {
            $this->processAdd(
                $id,
                post('gen_add_hod'),
                post('gen_add_len'),
                post('gen_add_od')
            );

            $items = DBRozpis::getRozpisItem($id);
        }

        if (post('remove') > 0) {
            DBRozpis::removeRozpisItem(post('remove'));

            $items = DBRozpis::getRozpisItem($id);
        }

        foreach ($items as $item) {
            if (!(post($item['ri_id'] . '-partner') == $item['ri_partner']
                || (post($item['ri_id'] . '-partner') == 'none' && $item['ri_partner'] == '0'))
                || post($item['ri_id'] . '-od') != formatTime($item['ri_od'], 1)
                || post($item['ri_id'] . '-do') != formatTime($item['ri_do'], 1)
                || (bool) post($item['ri_id'] . '-lock') != (bool) $item['ri_lock']
            ) {
                $partner = (post($item['ri_id'] . '-partner') == 'none')
                    ? '0'
                    : post($item['ri_id'] . '-partner');
                $od = post($item['ri_id'] . '-od');
                $do = post($item['ri_id'] . '-do');
                $lock = post($item['ri_id'] . '-lock') ? 1 : 0;

                if (preg_match('/^[0-9]{2}:[0-9]{2}$/', $od)
                    && preg_match('/^[0-9]{2}:[0-9]{2}$/', $do)
                ) {
                    DBRozpis::editRozpisItem(
                        $item['ri_id'], $partner,
                        formatTime($od, 0), formatTime($do, 0), $lock
                    );
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

        $this->displayForm($data, $users, $items);
    }

    private function displayForm()
    {
        $nabidky = DBNabidka::getNabidka();

        $newData = array(
            'id' => $data['r_id'],
            'fullName' => $data['u_jmeno'] . ' ' . $data['u_prijmeni'],
            'date' => formatDate($data['r_datum']),
            'kde' => $data['r_kde'],
            'showAdmin' => Permissions::check('rozpis', P_OWNED, $data['r_id'])
        );
        $data = $newData;

        foreach ($nabidky as &$item) {
            $newData = array(
                'id' => $item['n_id'],
                'date' => formatDate($item['n_od'])
                    . ($item['n_od'] != $item['n_do'] ?
                    (' - ' . formatDate($item['n_do'])) : ''),
                'fullName' => $item['u_jmeno'] . ' ' . $item['u_prijmeni']
            );
        } unset($item);

        foreach ($items as &$item) {
            $newData = array(
                'id' => $item['ni_id'],
                'fullName' => $item['u_jmeno'] . ' ' . $item['u_prijmeni'],
                'pocetHod' => $item['ni_pocet_hod']
            );
        } unset($item);

        if (post('n_id') && ($current = DBNabidka::getSingleNabidka(post('n_id')))) {
            $nabidkaItems = DBNabidka::getNabidkaItemLessons(post('n_id'));

            $newData = array(
                'id' => $current['n_id'],
                'fullName' => $current['u_jmeno'] . ' ' . $current['u_prijmeni'],
                'date' => formatDate($current['n_od'])
                    . ($current['n_od'] != $current['n_do'])
                    ? (' - ' .  formatDate($current['n_do'])) : '',
                'showAdmin' => Permissions::check('nabidka', P_OWNED, $current['n_trener']),
                'maxLessons' => $current['n_max_pocet_hodin'],
                'totalLessons' => $current['n_pocet_hod'],
                'occupied' => count($nabidkaItems)
            );
            $current = $newData;
        } else {
            $current = array();
        }
        $this->render(
            'src/application/View/Admin/Rozpis/Detail.inc',
            array(
                'data' => $data,
                'users' => $users,
                'items' => $items,
                'nabidky' => $nabidky,
                'currentNabidka' => $current
            )
        );
    }

    //TODO: Fix processOverlap()!
    private function processOverlap($items)
    {
        $end = "00:00";
        foreach ($items as $item) {
            $l_start = formatTime($item['ri_od'], 1);
            $l_end   = formatTime($item['ri_do'], 1);
            $length  = timeSubstract($l_end, $l_start);

            if (strcmp($end, $l_start) > 0) {
                $l_start = $end;
                $l_end = timeAdd($l_start, $length);
            }
            if (strcmp($l_start, $l_end) > 0) {
                $l_end = timeAdd($l_start, $length);
            }
            DBRozpis::editRozpisItem(
                $item['ri_id'], $item['ri_partner'],
                formatTime($l_start, 0), formatTime($l_end, 0), $item['ri_lock']
            );

            $end = $l_end;
        }
    }
    //TODO: Convert to DateTime object!
    private function processAdd($id, $count, $length, $from)
    {
        $length = floor($length / 60) . ':' . (int) ($length % 60);
        $to = timeAdd($from, $length);

        for ($i = 0; $i < $count; $i++) {
            DBRozpis::addRozpisItem(
                $id,
                '0',
                formatTime($from, 0),
                formatTime($to, 0),
                '0'
            );
            $from = $to;
            $to = timeAdd($from, $length);
        }
    }
}
?>