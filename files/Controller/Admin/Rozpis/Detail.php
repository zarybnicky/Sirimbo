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
            $this->redirect('/' . Request::getURI());
        }

        $users = DBPary::getPartners();

        $items = array_map(
            function ($item) {
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

        $nabidky = DBNabidka::getNabidka();
        usort(
            $nabidky,
            function ($a, $b) {
                $a1 = $a['u_prijmeni'] . $a['u_jmeno'] . $a['n_od'];
                $b1 = $b['u_prijmeni'] . $b['u_jmeno'] . $b['n_od'];
                return $a1 < $b1 ? -1 : ($a1 > $b1 ? 1 : 0);
            }
        );

        $nabidky_select = array();
        foreach ($nabidky as $item) {
            $nabidky_select[$item['n_id']] =
                $item['u_prijmeni'] . ', ' . $item['u_jmeno'] .
                ' (' . formatDate($item['n_od']) .
                (($item['n_od'] != $item['n_do']) ?
                 (' - ' . formatDate($item['n_do'])) :
                 '') .
                ')';
        }

        if (get('n') && ($nabidka = DBNabidka::getSingleNabidka(get('n')))) {
            $nabidka_items = array_map(
                function ($item) {
                    return array(
                        'fullName' => $item['u_jmeno'] . ' ' . $item['u_prijmeni'],
                        'lessonCount' => $item['ni_pocet_hod']
                    );
                },
                DBNabidka::getNabidkaItem(get('n'))
            );
            
            $obsazeno = array_reduce(
                $nabidka_items,
                function ($carry, $item) {
                    return $carry + $item['lessonCount'];
                },
                0
            );

            $nabidka = array(
                'id' => $nabidka['n_id'],
                'fullName' => $nabidka['u_jmeno'] . ' ' . $nabidka['u_prijmeni'],
                'datum' => formatDate($nabidka['n_od'])
                . ($nabidka['n_od'] != $nabidka['n_do'] ? ' - ' . formatDate($nabidka['n_do']) : ''),
                'canEdit' => false,
                'hourMax' => $nabidka['n_max_pocet_hod'],
                'hourTotal' => $nabidka['n_pocet_hod'],
                'hourReserved' => $obsazeno,
                'hourFree' => $nabidka['n_pocet_hod'] - $obsazeno
            );

            $nabidka['items'] = $nabidka_items;
        }

        $this->render(
            'files/View/Admin/Rozpis/Detail.inc',
            array(
                'data' => $data,
                'users' => $users,
                'items' => $items,
                'nabidky' => $nabidky_select,
                'nabidka' => isset($nabidka) ? $nabidka : array()
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
            $item['ri_partner'] = post($item['ri_id'] . '-partner');
            $item['ri_od'] = formatTime(post($item['ri_id'] . '-od'), 0);
            $item['ri_do'] = formatTime(post($item['ri_id'] . '-do'), 0);
            $item['ri_lock'] = post($item['ri_id'] . '-lock') ? 1 : 0;
        }    

        //Try to add a new item
        if (post('add_od') && post('add_do')) {
            if (is_object($f = $this->checkAdd())) {
                $this->redirect()->setMessage($f->getMessages());
            } else {
                $newId = DBRozpis::addRozpisItem(
                    $id,
                    post('add_partner'),
                    formatTime(post('add_od'), 0),
                    formatTime(post('add_do'), 0),
                    (int) (bool) post('add_lock')
                );
                $items[] = DBRozpis::getRozpisItemLesson($newId);
                
                post('add_partner', null);
            }
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

                $lastEnd = DateTimeImmutable::createFromFormat('H:i', '00:00');
                foreach ($items as &$item) {
                    $start = DateTimeImmutable::createFromFormat('H:i:s', $item['ri_od']);
                    $end = DateTimeImmutable::createFromFormat('H:i:s', $item['ri_do']);
                    $length = $start->diff($end);

                    if ($lastEnd > $start) {
                        $start = $lastEnd;
                        $end = $start->add($length);
                    }
                    if ($start > $end) {
                        $end = $start->add($length);
                    }
                    $lastEnd = $end;
                    
                    $item['ri_od'] = $start->format('H:i:s');
                    $item['ri_do'] = $end->format('H:i:s');
                }
                break;
                
            case 'add_multiple':
                if (is_object($f = $this->checkAddMultiple())) {
                    $this->redirect()->setMessage($f->getMessages());
                    break;
                }

                $start = DateTimeImmutable::createFromFormat('H:i', post('add_multi_od'));
                $length = new DateInterval('PT' . post('add_multi_len') . 'M');
                $end = $start->add($length);
                
                for($i = 0; $i < post('add_multi_num'); $i++) {
                    $newId = DBRozpis::addRozpisItem(
                        $id,
                        '0',
                        $start->format('H:i:s'),
                        $end->format('H:i:s'),
                        '0'
                    );
                    $items[] = DBRozpis::getRozpisItemLesson($newId);
                    
                    $start = $end;
                    $end = $start->add($length);
                }
                break;
        }

        return $items;
    }

    protected function checkAdd() {
        $f = new Form();

        $f->checkNumeric(
            post('add_partner'),
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
