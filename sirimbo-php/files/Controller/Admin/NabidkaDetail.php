<?php
namespace Olymp\Controller\Admin;

class NabidkaDetail
{
    public static function detail($id)
    {
        \Permissions::checkError('nabidka', P_OWNED);
        if (!$data = \DBNabidka::getSingleNabidka($id)) {
            new \MessageHelper('warning', 'Nabídka s takovým ID neexistuje');
            new \RedirectHelper('/admin/nabidka');
        }
        \Permissions::checkError('nabidka', P_OWNED, $data['n_trener']);

        $items = \DBNabidka::getNabidkaItem($id);
        $obsazeno = \DBNabidka::getNabidkaItemLessons($id);
        $users = \DBPary::getPartners();

        $userSelect = (new \UserSelectHelper($users))->type('par')->idVar('p_id');
        $items = array_map(fn($item) => [
            'user' => (string) $userSelect->set($item['ni_partner'])->name($item['ni_id'] . '-partner'),
            'lessonCount' => (new \TextHelper($item['ni_id'] . '-hodiny', $item['ni_pocet_hod']))->size(1),
            'removeButton' => (new \SubmitHelper('Odstranit'))->name('remove')->value($item['ni_id'])
        ], $items);
        $items[] = [
            'user' => (string) $userSelect->set(null) ->name('add_partner'),
            'lessonCount' => (new \TextHelper('add_hodiny', ''))->size('1'),
            'removeButton' => new \SubmitHelper('Přidat')
        ];
        return new \RenderHelper('files/View/Admin/Nabidka/Detail.inc', [
            'header' => 'Správa nabídky',
            'nabidka' => [
                'id' => $data['n_id'],
                'fullName' => $data['u_jmeno'] . ' ' . $data['u_prijmeni'],
                'datum' => (
                    \Format::date($data['n_od'])
                    . ($data['n_od'] != $data['n_do']
                       ? ' - ' . \Format::date($data['n_do'])
                       : '')
                ),
                'canEdit' => false,
                'hourMax' => $data['n_max_pocet_hod'],
                'hourTotal' => $data['n_pocet_hod'],
                'hourReserved' => $obsazeno ?: '',
                'hourFree' => $data['n_pocet_hod'] - $obsazeno
            ],
            'obsazeno' => $obsazeno,
            'users' => $users,
            'items' => $items,
            'backlink' => $_SERVER['HTTP_REFERER']
        ]);
    }

    public static function detailPost($id)
    {
        \Permissions::checkError('nabidka', P_OWNED);
        if (!$data = \DBNabidka::getSingleNabidka($id)) {
            new \MessageHelper('warning', 'Nabídka s takovým ID neexistuje');
            new \RedirectHelper('/admin/nabidka');
        }
        \Permissions::checkError('nabidka', P_OWNED, $data['n_trener']);

        $items = \DBNabidka::getNabidkaItem($id);
        $obsazeno = \DBNabidka::getNabidkaItemLessons($id);

        if ($_POST["remove"] > 0) {
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
                \DBNabidka::editNabidkaItem(
                    $item["ni_id"],
                    $partnerNew,
                    $countNew
                );
            }
        }
        $items = \DBNabidka::getNabidkaItem($id);
        $obsazeno = \DBNabidka::getNabidkaItemLessons($id);

        if (is_numeric($_POST["add_hodiny"]) &&
            is_numeric($_POST["add_partner"]) &&
            $_POST['add_partner']
        ) {
            $partner = $_POST['add_partner'];
            $count = $_POST["add_hodiny"];
            unset($_POST['add_partner']);
            unset($_POST['add_hodiny']);

            if (0 < $maxLessons && $maxLessons < $count) {
                $count = $maxLessons;
            }

            \DBNabidka::addNabidkaItemLessons($_POST["add_partner"], $id, $count);
            $items = \DBNabidka::getNabidkaItem($id);
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
            $data = \DBNabidka::getSingleNabidka($id);
        }
        new \RedirectHelper('/admin/nabidka/detail/' . $id);
    }
}
