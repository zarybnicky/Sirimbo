<?php
class Controller_Admin_Rozpis_Detail extends Controller_Abstract
{
    public function __construct()
    {
        Permissions::checkError('rozpis', P_OWNED);
    }

    public function view($request)
    {
        if (!$id = $request->getId()) {
            $this->redirect()->warning('Rozpis s takovým ID neexistuje');
            $this->redirect('/admin/rozpis');
        }
        if (!$data = DBRozpis::getSingleRozpis($id)) {
            $this->redirect()->warning('Rozpis s takovým ID neexistuje');
            $this->redirect('/admin/rozpis');
        }

        Permissions::checkError('rozpis', P_OWNED, $data['r_trener']);

        $items = DBRozpis::getRozpisItem($id);

        if ($request->post()) {
            $items = $this->processPost($request, $id, $data, $items);
            if ($items) {
                DBRozpis::editRozpisItemMultiple($items);
            }
            $this->redirect('/' . $request->getURI());
        }

        $users = DBPary::getPartners();

        $items = array_map(
            function ($item) {
                return [
                    'id' => $item['ri_id'],
                    'partner' => $item['ri_partner'],
                    'timeFrom' => formatTime($item['ri_od'], 1),
                    'timeTo' => formatTime($item['ri_do'], 1),
                    'lock' => (bool) $item['ri_lock']
                ];
            },
            $items
        );
        $data = [
            'id' => $data['r_id'],
            'datum' => formatDate($data['r_datum']),
            'kde' => $data['r_kde'],
            'fullName' => $data['u_jmeno'] . ' ' . $data['u_prijmeni'],
            'canEdit' => Permissions::check('nabidka', P_OWNED, $data['r_trener'])
        ];

        $nabidky = DBNabidka::getNabidka();
        usort(
            $nabidky,
            function ($a, $b) {
                $a1 = $a['u_prijmeni'] . $a['u_jmeno'] . $a['n_od'];
                $b1 = $b['u_prijmeni'] . $b['u_jmeno'] . $b['n_od'];
                return $a1 < $b1 ? -1 : ($a1 > $b1 ? 1 : 0);
            }
        );

        $nabidky_select = [];
        foreach ($nabidky as $item) {
            $nabidky_select[$item['n_id']] =
                $item['u_prijmeni'] . ', ' . $item['u_jmeno'] .
                ' (' . formatDate($item['n_od']) .
                (($item['n_od'] != $item['n_do']) ?
                 (' - ' . formatDate($item['n_do'])) :
                 '') .
                ')';
        }

        if ($request->get('n') && ($nabidka = DBNabidka::getSingleNabidka($request->get('n')))) {
            $nabidka_items = array_map(
                function ($item) {
                    return [
                        'fullName' => $item['u_jmeno'] . ' ' . $item['u_prijmeni'],
                        'lessonCount' => $item['ni_pocet_hod']
                    ];
                },
                DBNabidka::getNabidkaItem($request->get('n'))
            );

            $obsazeno = array_reduce(
                $nabidka_items,
                function ($carry, $item) {
                    return $carry + $item['lessonCount'];
                },
                0
            );

            $nabidka = [
                'id' => $nabidka['n_id'],
                'fullName' => $nabidka['u_jmeno'] . ' ' . $nabidka['u_prijmeni'],
                'datum' => formatDate($nabidka['n_od'])
                . ($nabidka['n_od'] != $nabidka['n_do'] ? ' - ' . formatDate($nabidka['n_do']) : ''),
                'canEdit' => false,
                'hourMax' => $nabidka['n_max_pocet_hod'],
                'hourTotal' => $nabidka['n_pocet_hod'],
                'hourReserved' => $obsazeno,
                'hourFree' => $nabidka['n_pocet_hod'] - $obsazeno
            ];

            $nabidka['items'] = $nabidka_items;
        }

        $this->render('files/View/Admin/Rozpis/Detail.inc', [
            'header' => 'Správa rozpisů',
            'data' => $data,
            'users' => $users,
            'items' => $items,
            'selected_nabidka' => $request->get('n') ?: '',
            'nabidky' => $nabidky_select,
            'nabidka' => isset($nabidka) ? $nabidka : []
        ]);
    }

    protected function processPost($request, $id, $data, $items)
    {
        if ($request->post('remove') > 0) {
            DBRozpis::removeRozpisItem($request->post('remove'));
            $items = DBRozpis::getRozpisItem($id);
        }
        //Update all
        foreach ($items as &$item) {
            $item['ri_partner'] = $request->post($item['ri_id'] . '-partner');
            $item['ri_od'] = formatTime($request->post($item['ri_id'] . '-od'), 0);
            $item['ri_do'] = formatTime($request->post($item['ri_id'] . '-do'), 0);
            $item['ri_lock'] = $request->post($item['ri_id'] . '-lock') ? 1 : 0;
        }

        //Try to add a new item
        if ($request->post('add_od') && $request->post('add_do')) {
            $form = $this->checkAdd($request);
            if (!$form->isValid()) {
                $this->redirect()->warning($form->getMessages());
            } else {
                $newId = DBRozpis::addRozpisItem(
                    $id,
                    $request->post('add_partner'),
                    formatTime($request->post('add_od'), 0),
                    formatTime($request->post('add_do'), 0),
                    (int) (bool) $request->post('add_lock')
                );
                $items[] = DBRozpis::getRozpisItemLesson($newId);

                $request->post('add_partner', null);
            }
        }

        switch ($request->post('action')) {
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

                $lastEnd = new DateTime('00:00');
                foreach ($items as &$item) {
                    $start = DateTime::createFromFormat('H:i:s', $item['ri_od']);
                    $end = DateTime::createFromFormat('H:i:s', $item['ri_do']);
                    if (!$start || !$end) {
                        break;
                    }
                    $length = $start->diff($end);

                    if ($lastEnd > $start) {
                        $start = clone $lastEnd;
                        $end = clone $start;
                        $end->add($length);
                    }
                    if ($start > $end) {
                        $end = clone $start;
                        $end->add($length);
                    }
                    $lastEnd = $end;

                    $item['ri_od'] = $start->format('H:i:s');
                    $item['ri_do'] = $end->format('H:i:s');
                }
                break;

            case 'add_multiple':
                $form = $this->checkAddMultiple($request);
                if (!$form->isValid()) {
                    $this->redirect()->warning($form->getMessages());
                    break;
                }

                $start = DateTime::createFromFormat('H:i', $request->post('add_multi_od'));
                $length = new DateInterval('PT' . $request->post('add_multi_len') . 'M');
                if (!$start) {
                    break;
                }
                $end = clone $start;
                $end->add($length);

                for ($i = 0; $i < $request->post('add_multi_num'); $i++) {
                    $newId = DBRozpis::addRozpisItem(
                        $id,
                        '0',
                        $start->format('H:i:s'),
                        $end->format('H:i:s'),
                        '0'
                    );
                    $items[] = DBRozpis::getRozpisItemLesson($newId);

                    $start = $end;
                    $end = clone $start;
                    $end->add($length);
                }
                break;
        }

        return $items;
    }

    protected function checkAdd($request): Form
    {
        $f = new Form();
        $f->checkNumeric(
            $request->post('add_partner'),
            'Neplatný partner u přidávané lekce',
            'add_partner'
        );
        $f->checkTime(
            $request->post('add_od'),
            'Neplatný formát času "od" u přidávané lekce',
            'add_od'
        );
        $f->checkTime(
            $request->post('add_do'),
            'Neplatný formát času "do" u přidávané lekce',
            'add_do'
        );
        return $f;
    }

    protected function checkAddMultiple($request): Form
    {
        $f = new Form();
        $f->checkNumeric(
            $request->post('add_multi_num'),
            'Neplatný počet přidávaných hodin',
            'add_multi_num'
        );
        $f->checkNumeric(
            $request->post('add_multi_len'),
            'Neplatná délka přidávaných hodin',
            'add_multi_len'
        );
        $f->checkTime(
            $request->post('add_multi_od'),
            'Neplatný formát času "od" u přidávaných hodin',
            'add_multi_od'
        );
        return $f;
    }
}
