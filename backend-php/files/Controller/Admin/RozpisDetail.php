<?php
namespace Olymp\Controller\Admin;

class RozpisDetail
{
    public static function detail($id)
    {
        \Permissions::checkError('rozpis', P_OWNED);
        $data = \Database::querySingle(
            "SELECT r_id,r_trener,u_jmeno,u_prijmeni,r_kde,r_datum,r_visible,r_lock FROM rozpis LEFT JOIN users ON r_trener=u_id WHERE r_id='?'",
            $id,
        );
        if (!$data) {
            \Message::warning('Rozpis s takovým ID neexistuje');
            \Redirect::to('/admin/rozpis');
        }
        \Permissions::checkError('rozpis', P_OWNED, $data['r_trener']);
        $items = \Database::queryArray(
            "SELECT p_id,u_id,u_login,u_jmeno,u_prijmeni,ri_id,ri_id_rodic,ri_partner,ri_od,ri_do,ri_lock
            FROM rozpis_item LEFT JOIN pary ON ri_partner=p_id LEFT JOIN users ON p_id_partner=u_id
            WHERE ri_id_rodic='?'
            ORDER BY ri_od",
            $id,
        );
        $users = \DBPary::getPartners(array_filter(array_column($items, 'p_id')));
        $data = $data + [
            'canEdit' => \Permissions::check('nabidka', P_OWNED, $data['r_trener'])
        ];

        \Render::twig('Admin/RozpisDetail.twig', [
            'data' => $data,
            'users' => $users,
            'items' => $items
        ]);
    }
}
