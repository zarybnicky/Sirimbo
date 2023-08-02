<?php
namespace Olymp\Controller\Admin;

class Rozpis
{
    public static function list()
    {
        \Permissions::checkError('rozpis', P_OWNED);
        \Render::twig('Admin/Rozpis.twig', [
            'data' => array_map(
                fn($item) => [
                    'id' => $item['r_id'],
                    'fullName' => "{$item['u_jmeno']} {$item['u_prijmeni']}",
                    'date' => $item['r_datum'],
                    'kde' => $item['r_kde'],
                    'visible' => $item['r_visible'],
                ],
                \Permissions::check('rozpis', P_ADMIN)
                ? \Database::queryArray(
                    "SELECT u_jmeno,u_prijmeni,r_id,r_trener,r_kde,r_datum,r_visible,r_lock" .
                    " FROM rozpis LEFT JOIN users ON r_trener=u_id ORDER BY r_datum DESC"
                )
                : \Database::queryArray(
                    "SELECT u_jmeno,u_prijmeni,r_id,r_trener,r_kde,r_datum,r_visible,r_lock" .
                    " FROM rozpis LEFT JOIN users ON r_trener=u_id WHERE r_trener='?' ORDER BY r_datum DESC",
                    \Session::getUser()->getId()
                )
            )
        ]);
    }

    public static function edit($id)
    {
        $data = \Database::querySingle(
            "SELECT r_id,r_trener,u_jmeno,u_prijmeni,r_kde,r_datum,r_visible,r_lock FROM rozpis LEFT JOIN users ON r_trener=u_id WHERE r_id='?'",
            $id,
        );
        if (!$data) {
            \Message::warning('Rozpis s takovým ID neexistuje');
            \Redirect::to('/admin/rozpis');
        }
        \Permissions::checkError('rozpis', P_OWNED, $data['r_trener']);
        return self::displayForm($data);
    }

    protected static function displayForm($data = null)
    {
        \Render::twig('Admin/RozpisForm.twig', [
            'action' => $data ? 'edit' : 'add',
            'treneri' => \Permissions::check('rozpis', P_ADMIN)
            ? \DBUser::getUsersByPermission('rozpis', P_OWNED)
            : [\DBUser::getUserData(\Session::getUser()->getId())],
            'trener' => $_POST['trener'] ?? ($data ? $data['r_trener'] : ''),
            'kde' => $_POST['kde'] ?? ($data ? $data['r_kde'] : ''),
            'datum' => $_POST['datum'] ?? ($data ? $data['r_datum'] : ''),
            'visible' => $_POST['visible'] ?? ($data ? $data['r_visible'] : ''),
            'lock' => $_POST['lock'] ?? ($data ? $data['r_lock'] : '')
        ]);
    }
}
