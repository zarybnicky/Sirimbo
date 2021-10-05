<?php
namespace Olymp\Controller\Admin;

class RozpisDetail
{
    public static function detail($id)
    {
        \Permissions::checkError('rozpis', P_OWNED);
        if (!$data = \DBRozpis::getSchedule($id)) {
            \Message::warning('Rozpis s takovým ID neexistuje');
            \Redirect::to('/admin/rozpis');
        }
        \Permissions::checkError('rozpis', P_OWNED, $data['r_trener']);
        $items = \DBRozpis::getLessons($id);
        $users = \DBPary::getPartners(array_column($items, 'p_id'));

        $data = $data + [
            'canEdit' => \Permissions::check('nabidka', P_OWNED, $data['r_trener'])
        ];

        $nabidky = \DBNabidka::getNabidka();
        usort(
            $nabidky,
            function ($a, $b) {
                $a1 = $a['n_od'] . $a['u_prijmeni'] . $a['u_jmeno'];
                $b1 = $b['n_od'] . $b['u_prijmeni'] . $b['u_jmeno'];
                return $a1 < $b1 ? 1 : ($a1 > $b1 ? -1 : 0);
            }
        );

        \Render::twig('Admin/RozpisDetail.twig', [
            'data' => $data,
            'users' => $users,
            'items' => $items,
            'nabidky' => array_for($nabidky, fn($item) => [
                'key' => $item['n_id'],
                'value' => (
                    date('j. n. Y', strtotime($item['n_od'])) .
                    (($item['n_od'] != $item['n_do']) ?
                     (' - ' . date('j. n. Y', strtotime($item['n_do']))) : '') .
                    " - {$item['u_jmeno']} {$item['u_prijmeni']}"
                ),
            ]),
        ]);
    }

    public static function detailPost($id)
    {
        \Permissions::checkError('rozpis', P_OWNED);
        if (!$data = \DBRozpis::getSchedule($id)) {
            \Message::warning('Rozpis s takovým ID neexistuje');
            \Redirect::to('/admin/rozpis');
        }
        \Permissions::checkError('rozpis', P_OWNED, $data['r_trener']);
        $items = static::processPost($id, $data, \DBRozpis::getLessons($id));
        if ($items) {
            \DBRozpis::editMultipleLessons($items);
        }
        \Redirect::to($_SERVER['REQUEST_URI']);
    }

    protected static function processPost($id, $data, $items)
    {
        if (($_POST['remove'] ?? null) > 0) {
            \DBRozpis::deleteLesson($_POST['remove']);
            $items = \DBRozpis::getLessons($id);
        }
        //Update all
        foreach ($items as &$item) {
            $item['ri_partner'] = $_POST[$item['ri_id'] . '-partner'];
            $item['ri_od'] = $_POST[$item['ri_id'] . '-od'] . ':00';
            $item['ri_do'] = $_POST[$item['ri_id'] . '-do'] . ':00';
            $item['ri_lock'] = ($_POST[$item['ri_id'] . '-lock'] ?? '') ? 1 : 0;
        }

        //Try to add a new item
        if ($_POST['add_od'] && $_POST['add_do']) {
            $form = static::checkAdd();
            if (!$form->isValid()) {
                \Message::warning($form->getMessages());
            } else {
                $newId = \DBRozpis::addLesson(
                    $id,
                    $_POST['add_partner'],
                    $_POST['add_od'] . ':00',
                    $_POST['add_do'] . ':00',
                    (int) (bool) $_POST['add_lock']
                );
                $items[] = \DBRozpis::getLesson($newId);
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
                    \Message::warning($form->getMessages());
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
                    $newId = \DBRozpis::addLesson(
                        $id,
                        '0',
                        $start->format('H:i:s'),
                        $end->format('H:i:s'),
                        '0'
                    );
                    $items[] = \DBRozpis::getLesson($newId);

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
        $f->checkNumeric($_POST['add_partner'], 'Neplatný partner u přidávané lekce');
        $f->checkTime($_POST['add_od'], 'Neplatný formát času "od" u přidávané lekce');
        $f->checkTime($_POST['add_do'], 'Neplatný formát času "do" u přidávané lekce');
        return $f;
    }

    protected static function checkAddMultiple(): \Form
    {
        $f = new \Form();
        $f->checkNumeric($_POST['add_multi_num'], 'Neplatný počet přidávaných hodin');
        $f->checkNumeric($_POST['add_multi_len'], 'Neplatná délka přidávaných hodin');
        $f->checkTime($_POST['add_multi_od'], 'Neplatný formát času "od" u přidávaných hodin');
        return $f;
    }
}
