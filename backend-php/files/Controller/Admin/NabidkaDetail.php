<?php
namespace Olymp\Controller\Admin;

class NabidkaDetail
{
    public static function detail($id)
    {
        \Permissions::checkError('nabidka', P_OWNED);
        $data = \Database::querySingle("SELECT n_id, u_jmeno, u_prijmeni, nabidka.* FROM nabidka LEFT JOIN users ON n_trener=u_id WHERE n_id='?'", $id);
        if (!$data) {
            \Message::warning('Nabídka s takovým ID neexistuje');
            \Redirect::to('/admin/nabidka');
        }
        \Permissions::checkError('nabidka', P_OWNED, $data['n_trener']);
        $items = \DBNabidka::getReservationItems($id);
        \Render::twig('Admin/NabidkaDetail.twig', [
            'nabidka' => $data + [
                'hourReserved' => \Database::querySingle("SELECT SUM(ni_pocet_hod) as sum FROM nabidka_item WHERE ni_id_rodic='?'", $id)['sum'],
                'canEdit' => true,
            ],
            'users' => \DBPary::getPartners(array_filter(array_column($items, 'p_id'))),
            'items' => $items,
            'backlink' => $_SERVER['HTTP_REFERER']
        ]);
    }

    public static function detailPost($id)
    {
        \Permissions::checkError('nabidka', P_OWNED);
        $data = \Database::querySingle("SELECT * FROM nabidka WHERE n_id='?'", $id);
        if (!$data) {
            \Message::warning('Nabídka s takovým ID neexistuje');
            \Redirect::to('/admin/nabidka');
        }
        \Permissions::checkError('nabidka', P_OWNED, $data['n_trener']);

        if (($_POST["remove"] ?? 0) > 0) {
            \Database::query("DELETE FROM nabidka_item WHERE ni_id_rodic='?' AND ni_partner='?'", $id, $_POST[$_POST["remove"] . "-partner"]);
        }

        $items = \DBNabidka::getReservationItems($id);
        $maxLessons = $data['n_max_pocet_hodin'];

        foreach ($items as $item) {
            $partner = $item["ni_partner"];
            $partnerNew = $_POST[$item["ni_id"] . "-partner"];
            $count = $item['ni_pocet_hod'];
            $countNew = $_POST[$item["ni_id"] . "-hodiny"];

            if ($partner != $partnerNew || $count != $countNew) {
                if (0 < $maxLessons && $maxLessons < $countNew) {
                    $countNew = $maxLessons;
                }
                \DBNabidka::editNabidkaItem($item["ni_id"], $partnerNew, $countNew);
            }
        }

        if (is_numeric($_POST["add_hodiny"] ?? null) &&
            is_numeric($_POST["add_partner"] ?? null) &&
            $_POST['add_partner']
        ) {
            $count = $_POST["add_hodiny"];
            if (0 < $maxLessons && $maxLessons < $count) {
                $count = $maxLessons;
            }

            \DBNabidka::addNabidkaItemLessons($_POST["add_partner"], $id, $count);
        }

        //-----Dorovnávání skutečného a nastaveného počtu hodin-----//
        $obsazeno = \Database::querySingle("SELECT SUM(ni_pocet_hod) as sum FROM nabidka_item WHERE ni_id_rodic='?'", $id)['sum'];
        if ($obsazeno > $data["n_pocet_hod"]) {
            \Database::query("UPDATE nabidka SET n_pocet_hod='?' WHERE n_id='?'", $obsazeno, $id);
        }
        \Redirect::to('/admin/nabidka/detail/' . $id);
    }
}
