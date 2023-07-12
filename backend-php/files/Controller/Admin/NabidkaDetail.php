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
        $items = \Database::queryArray(
            "SELECT p_id,u_id,u_jmeno,u_prijmeni,nabidka_item.* FROM nabidka_item LEFT JOIN pary ON ni_partner=p_id LEFT JOIN users ON p_id_partner=u_id WHERE ni_id_rodic='?'",
            $id
        );
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

        $items = \Database::queryArray(
            "SELECT p_id,u_id,u_jmeno,u_prijmeni,nabidka_item.* FROM nabidka_item LEFT JOIN pary ON ni_partner=p_id LEFT JOIN users ON p_id_partner=u_id WHERE ni_id_rodic='?'",
            $id
        );
        $maxLessons = $data['n_max_pocet_hodin'];

        foreach ($items as $item) {
            $count = $item['ni_pocet_hod'];
            $countNew = $_POST[$item["ni_id"] . "-hodiny"];

            if ($count != $countNew) {
                if (0 < $maxLessons && $maxLessons < $countNew) {
                    $countNew = $maxLessons;
                }
                \Database::query("UPDATE nabidka_item SET ni_pocet_hod='?' WHERE ni_id='?'", $countNew, $item['ni_id']);
            }
        }

        if (is_numeric($_POST["add_hodiny"] ?? null) && is_numeric($_POST["add_partner"] ?? null) && $_POST['add_partner']) {
            $count = $_POST["add_hodiny"];
            if (0 < $maxLessons && $maxLessons < $count) {
                $count = $maxLessons;
            }
            \Database::query(
                "INSERT INTO nabidka_item (ni_partner,ni_id_rodic,ni_pocet_hod) VALUES ('?','?','?') ON CONFLICT (ni_id_rodic, ni_partner) DO UPDATE SET ni_pocet_hod=nabidka_item.ni_pocet_hod+EXCLUDED.ni_pocet_hod",
                $_POST['add_partner'],
                $id,
                $count,
            );
        }

        \Redirect::to('/admin/nabidka/detail/' . $id);
    }
}
