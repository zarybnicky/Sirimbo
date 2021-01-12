<?php
namespace Olymp\Controller\Admin;

class NabidkaDetail
{
    public static function detail($id)
    {
        \Permissions::checkError('nabidka', P_OWNED);
        if (!$data = \DBNabidka::getSingleNabidka($id)) {
            \Message::warning('Nabídka s takovým ID neexistuje');
            \Redirect::to('/admin/nabidka');
        }
        \Permissions::checkError('nabidka', P_OWNED, $data['n_trener']);
        $items = \DBNabidka::getNabidkaItem($id);
        \Render::twig('Admin/NabidkaDetail.twig', [
            'nabidka' => [
                'id' => $data['n_id'],
                'fullName' => "{$data['u_jmeno']} {$data['u_prijmeni']}",
                'date' => $data['n_od'],
                'dateEnd' => $data['n_do'],
                'hourMax' => $data['n_max_pocet_hod'],
                'hourTotal' => $data['n_pocet_hod'],
                'hourReserved' => \DBNabidka::getNabidkaItemLessons($id),
                'canEdit' => true,
            ],
            'users' => \DBPary::getPartners(array_column($items, 'p_id')),
            'items' => $items,
            'backlink' => $_SERVER['HTTP_REFERER']
        ]);
    }

    public static function detailPost($id)
    {
        \Permissions::checkError('nabidka', P_OWNED);
        if (!$data = \DBNabidka::getSingleNabidka($id)) {
            \Message::warning('Nabídka s takovým ID neexistuje');
            \Redirect::to('/admin/nabidka');
        }
        \Permissions::checkError('nabidka', P_OWNED, $data['n_trener']);

        $items = \DBNabidka::getNabidkaItem($id);
        $obsazeno = \DBNabidka::getNabidkaItemLessons($id);

        if (($_POST["remove"] ?? 0) > 0) {
            \DBNabidka::removeNabidkaItem($id, $_POST[$_POST["remove"] . "-partner"]);
            $items = \DBNabidka::getNabidkaItem($id);
            $obsazeno = \DBNabidka::getNabidkaItemLessons($id);
        }

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
        $obsazeno = \DBNabidka::getNabidkaItemLessons($id);

        if (is_numeric($_POST["add_hodiny"] ?? null) &&
            is_numeric($_POST["add_partner"] ?? null) &&
            $_POST['add_partner']
        ) {
            $partner = $_POST['add_partner'];
            $count = $_POST["add_hodiny"];
            if (0 < $maxLessons && $maxLessons < $count) {
                $count = $maxLessons;
            }

            \DBNabidka::addNabidkaItemLessons($_POST["add_partner"], $id, $count);
            $obsazeno = \DBNabidka::getNabidkaItemLessons($id);
        }

        //-----Dorovnávání skutečného a nastaveného počtu hodin-----//
        if ($obsazeno > $data["n_pocet_hod"]) {
            \DBNabidka::editNabidka(
                $id,
                $data["n_trener"],
                $obsazeno,
                $data['n_max_pocet_hod'],
                $data["n_od"],
                $data["n_do"],
                $data['n_visible'],
                ($data["n_lock"]) ? 1 : 0
            );
        }
        \Redirect::to('/admin/nabidka/detail/' . $id);
    }
}
