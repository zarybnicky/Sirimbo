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
}
