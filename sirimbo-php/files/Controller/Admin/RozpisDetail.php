<?php
namespace Olymp\Controller\Admin;

class RozpisDetail
{
    public static function detail($id)
    {
        \Permissions::checkError('rozpis', P_OWNED);
        if (!$data = \DBRozpis::getSingleRozpis($id)) {
            new \MessageHelper('warning', 'Rozpis s takovým ID neexistuje');
            new \RedirectHelper('/admin/rozpis');
        }
        \Permissions::checkError('rozpis', P_OWNED, $data['r_trener']);
        $users = \DBPary::getPartners();
        $items = array_map(
            fn($item) => [
                'id' => $item['ri_id'],
                'partner' => $item['ri_partner'],
                'timeFrom' => \Format::time($item['ri_od'], 1),
                'timeTo' => \Format::time($item['ri_do'], 1),
                'lock' => (bool) $item['ri_lock']
            ],
            \DBRozpis::getRozpisItem($id),
        );
        $data = [
            'id' => $data['r_id'],
            'datum' => \Format::date($data['r_datum']),
            'kde' => $data['r_kde'],
            'fullName' => $data['u_jmeno'] . ' ' . $data['u_prijmeni'],
            'canEdit' => \Permissions::check('nabidka', P_OWNED, $data['r_trener'])
        ];

        $nabidky = \DBNabidka::getNabidka();
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
                ' (' . \Format::date($item['n_od']) .
                (($item['n_od'] != $item['n_do']) ?
                 (' - ' . \Format::date($item['n_do'])) :
                 '') .
                ')';
        }

        if ($_GET['n'] && ($nabidka = \DBNabidka::getSingleNabidka($_GET['n']))) {
            $nabidka_items = array_map(
                fn($item) => [
                    'fullName' => $item['u_jmeno'] . ' ' . $item['u_prijmeni'],
                    'lessonCount' => $item['ni_pocet_hod']
                ],
                \DBNabidka::getNabidkaItem($_GET['n'])
            );
            $obsazeno = array_reduce(
                $nabidka_items,
                fn($carry, $item) => $carry + $item['lessonCount'],
                0
            );
            $nabidka = [
                'id' => $nabidka['n_id'],
                'fullName' => $nabidka['u_jmeno'] . ' ' . $nabidka['u_prijmeni'],
                'datum' => \Format::date($nabidka['n_od'])
                . ($nabidka['n_od'] != $nabidka['n_do'] ? ' - ' . \Format::date($nabidka['n_do']) : ''),
                'canEdit' => false,
                'hourMax' => $nabidka['n_max_pocet_hod'],
                'hourTotal' => $nabidka['n_pocet_hod'],
                'hourReserved' => $obsazeno,
                'hourFree' => $nabidka['n_pocet_hod'] - $obsazeno,
                'items' => $nabidka_items,
            ];
        }

        new \RenderHelper('files/View/Admin/Rozpis/Detail.inc', [
            'header' => 'Správa rozpisů',
            'data' => $data,
            'users' => $users,
            'items' => $items,
            'selected_nabidka' => $_GET['n'] ?? '',
            'nabidky' => $nabidky_select,
            'nabidka' => isset($nabidka) ? $nabidka : []
        ]);
    }

    public static function detailPost($id)
    {
        \Permissions::checkError('rozpis', P_OWNED);
        if (!$data = \DBRozpis::getSingleRozpis($id)) {
            new \MessageHelper('warning', 'Rozpis s takovým ID neexistuje');
            new \RedirectHelper('/admin/rozpis');
        }
        \Permissions::checkError('rozpis', P_OWNED, $data['r_trener']);
        $items = static::processPost($id, $data, \DBRozpis::getRozpisItem($id));
        if ($items) {
            \DBRozpis::editRozpisItemMultiple($items);
        }
        new \RedirectHelper($_SERVER['REQUEST_URI']);
    }

    protected static function processPost($id, $data, $items)
    {
        if ($_POST['remove'] > 0) {
            \DBRozpis::removeRozpisItem($_POST['remove']);
            $items = \DBRozpis::getRozpisItem($id);
        }
        //Update all
        foreach ($items as &$item) {
            $item['ri_partner'] = $_POST[$item['ri_id'] . '-partner'];
            $item['ri_od'] = \Format::time($_POST[$item['ri_id'] . '-od'], 0);
            $item['ri_do'] = \Format::time($_POST[$item['ri_id'] . '-do'], 0);
            $item['ri_lock'] = $_POST[$item['ri_id'] . '-lock'] ? 1 : 0;
        }

        //Try to add a new item
        if ($_POST['add_od'] && $_POST['add_do']) {
            $form = static::checkAdd();
            if (!$form->isValid()) {
                new \MessageHelper('warning', $form->getMessages());
            } else {
                $newId = \DBRozpis::addRozpisItem(
                    $id,
                    $_POST['add_partner'],
                    \Format::time($_POST['add_od'], 0),
                    \Format::time($_POST['add_do'], 0),
                    (int) (bool) $_POST['add_lock']
                );
                $items[] = \DBRozpis::getRozpisItemLesson($newId);

                unset($_POST['add_partner']);
            }
        }

        switch ($_POST['action']) {
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

                $lastEnd = new \DateTime('00:00');
                foreach ($items as &$item) {
                    $start = \DateTime::createFromFormat('H:i:s', $item['ri_od']);
                    $end = \DateTime::createFromFormat('H:i:s', $item['ri_do']);
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
                $form = static::checkAddMultiple();
                if (!$form->isValid()) {
                    new \MessageHelper('warning', $form->getMessages());
                    break;
                }

                $start = \DateTime::createFromFormat('H:i', $_POST['add_multi_od']);
                $length = new \DateInterval('PT' . $_POST['add_multi_len'] . 'M');
                if (!$start) {
                    break;
                }
                $end = clone $start;
                $end->add($length);

                for ($i = 0; $i < $_POST['add_multi_num']; $i++) {
                    $newId = \DBRozpis::addRozpisItem(
                        $id,
                        '0',
                        $start->format('H:i:s'),
                        $end->format('H:i:s'),
                        '0'
                    );
                    $items[] = \DBRozpis::getRozpisItemLesson($newId);

                    $start = $end;
                    $end = clone $start;
                    $end->add($length);
                }
                break;
        }

        return $items;
    }

    protected static function checkAdd(): \Form
    {
        $f = new \Form();
        $f->checkNumeric(
            $_POST['add_partner'],
            'Neplatný partner u přidávané lekce',
            'add_partner'
        );
        $f->checkTime(
            $_POST['add_od'],
            'Neplatný formát času "od" u přidávané lekce',
            'add_od'
        );
        $f->checkTime(
            $_POST['add_do'],
            'Neplatný formát času "do" u přidávané lekce',
            'add_do'
        );
        return $f;
    }

    protected static function checkAddMultiple(): \Form
    {
        $f = new \Form();
        $f->checkNumeric(
            $_POST['add_multi_num'],
            'Neplatný počet přidávaných hodin',
            'add_multi_num'
        );
        $f->checkNumeric(
            $_POST['add_multi_len'],
            'Neplatná délka přidávaných hodin',
            'add_multi_len'
        );
        $f->checkTime(
            $_POST['add_multi_od'],
            'Neplatný formát času "od" u přidávaných hodin',
            'add_multi_od'
        );
        return $f;
    }
}