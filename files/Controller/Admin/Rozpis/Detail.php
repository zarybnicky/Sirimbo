<?php
require_once 'files/Controller/Admin/Rozpis.php';
class Controller_Admin_Rozpis_Detail extends Controller_Admin_Rozpis
{
    public function __construct() {
        Permissions::checkError('rozpis', P_OWNED);
    }
    public function view($id = null) {
        if (!$id || !($data = DBRozpis::getSingleRozpis($id))) {
            $this->redirect(
                '/admin/rozpis',
                'Rozpis s takovým ID neexistuje'
            );
        }

        Permissions::checkError('rozpis', P_OWNED, $data['r_trener']);

        $items = DBRozpis::getRozpisItem($id);

        if (post()) {
            $items = $this->processPost($id, $data, $items);
            DBRozpis::editRozpisItemMultiple($items);
        }

        $users = DBPary::getPartners();

        $items = array_map(
            function ($item) use ($data) {
                return array(
                    'id' => $item['ri_id'],
                    'partner' => $item['ri_partner'],
                    'timeFrom' => formatTime($item['ri_od'], 1),
                    'timeTo' => formatTime($item['ri_do'], 1),
                    'lock' => (bool) $item['ri_lock']
                );
            },
            $items
        );
        $data = array(
            'id' => $data['r_id'],
            'datum' => formatDate($data['r_datum']),
            'kde' => $data['r_kde'],
            'fullName' => $data['u_jmeno'] . ' ' . $data['u_prijmeni'],
            'canEdit' => Permissions::check('nabidka', P_OWNED, $data['r_trener'])
        );

        $this->render(
            'files/View/Admin/Rozpis/Detail.inc',
            array(
                'data' => $data,
                'users' => $users,
                'items' => $items
            )
        );
    }

    protected function processPost($id, $data, $items) {
        if (post('remove') > 0) {
            DBRozpis::removeRozpisItem(post('remove'));
            $items = DBRozpis::getRozpisItem($id);
        }

        //Update all
        foreach ($items as &$item) {
            $item['ri_partner'] = (post($item['ri_id'] . '-partner') == 'none')
                ? '0'
                : post($item['ri_id'] . '-partner');
            $item['ri_od'] = formatTime(post($item['ri_id'] . '-od'), 0);
            $item['ri_do'] = formatTime(post($item['ri_id'] . '-do'), 0);
            $item['ri_lock'] = post($item['ri_id'] . '-lock') ? 1 : 0;
        }    

        //Try to add a new item
        if (post('add_od') && post('add_do')) {
            $this->checkAdd();
            //XXX

            $newId = DBRozpis::addRozpisItem(
                $id,
                (post('add_partner') == 'none') ? "0" : post('add_partner'),
                formatTime(post('add_od'), 0),
                formatTime(post('add_do'), 0),
                (int) post('add_lock')
            );
            $items[] = DBRozpis::getRozpisItemLesson($newId);

            post('add_partner', null);
        }
        
        switch (post('action')) {
            case 'overlap':
                //Sort by begin time
                usort(
                    $items,
                    function ($a, $b) {
                        $a = $a['ri_od'];
                        $b = $b['ri_od'];
                        return $a < $b ? -1 : ($a == $b ? 0 : 1);
                    }
                );

                $end = "00:00";
                foreach ($items as $item) {
                    $l_start = formatTime($item['ri_od'], 1);
                    $l_end = formatTime($item['ri_do'], 1);
                    $length = timeSubstract($l_end, $l_start);
                    
                    if (strcmp($end, $l_start) > 0) {
                        $l_start = $end;
                        $l_end = timeAdd($l_start, $length);
                    }
                    if (strcmp($l_start, $l_end) > 0) {
                        $l_end = timeAdd($l_start, $length);
                    }
                    
                    $item['ri_od'] = formatTime($l_start, 0);
                    $item['ri_do'] = formatTime($l_end, 0);
                    
                    $end = $l_end;
                }
                break;
            case 'add_multiple':
                $this->checkAddMultiple();
                //XXX

                $od = post('add_multi_od');
                $len = floor(post('add_multi_len') / 60) . ':' .
                    (int)(post('add_multi_len') % 60);
                $do = timeAdd($od, $len);
                
                for($i = 0; $i < post('add_multi_num'); $i++) {
                    $newId = DBRozpis::addRozpisItem(
                        $id,
                        '0',
                        formatTime($od, 0),
                        formatTime($do, 0),
                        '0'
                    );
                    $items[] = DBRozpis::getRozpisItemLesson($newId);

                    $od = $do;
                    $do = timeAdd($od, $len);
                }
                break;
        }

        return $items;
    }

    protected function checkAdd() {
        $f = new Form();

        $f->checkBool(
            is_numeric(post('add_partner')) || post('add_partner') == 'none',
            'Neplatný partner u přidávané lekce',
            'add_partner'
        );
        $f->checkTime(
            post('add_od'),
            'Neplatný formát času "od" u přidávané lekce',
            'add_od'
        );
        $f->checkTime(
            post('add_do'),
            'Neplatný formát času "do" u přidávané lekce',
            'add_do'
        );

        return $f->isValid() ? true : $f;
    }

    protected function checkAddMultiple() {
        $f = new Form();

        $f->checkNumeric(
            post('add_multi_num'),
            'Neplatný počet přidávaných hodin',
            'add_multi_num'
        );
        $f->checkNumeric(
            post('add_multi_len'),
            'Neplatná délka přidávaných hodin',
            'add_multi_len'
        );
        $f->checkTime(
            post('add_multi_od'),
            'Neplatný formát času "od" u přidávaných hodin',
            'add_multi_od'
        );

        return $f->isValid() ? true : $f;
    }
}
