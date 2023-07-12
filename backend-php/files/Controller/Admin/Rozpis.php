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

    public static function add()
    {
        \Permissions::checkError('rozpis', P_OWNED);
        return self::displayForm();
    }

    public static function addPost()
    {
        \Permissions::checkError('rozpis', P_OWNED);
        $form = self::checkData();
        if (!$form->isValid()) {
            \Message::warning($form->getMessages());
            return self::displayForm();
        }
        \Permissions::checkError('rozpis', P_OWNED, $_POST['trener']);
        \Database::query(
            "INSERT INTO rozpis (r_trener,r_kde,r_datum,r_visible,r_lock) VALUES ('?','?','?','?','?')",
            $_POST['trener'],
            $_POST['kde'],
            (string) new \Date($_POST['datum'] ?? null),
            $_POST['visible'] ? '1' : '0',
            $_POST['lock'] ? '1' : '0'
        );
        \Redirect::to('/admin/rozpis');
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

    public static function editPost($id)
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
        $form = self::checkData();
        if (!$form->isValid()) {
            \Message::warning($form->getMessages());
            return self::displayForm($data);
        }
        \Database::query(
            "UPDATE rozpis SET r_trener='?',r_kde='?',r_datum='?',r_visible='?',r_lock='?' WHERE r_id='?'",
            $_POST['trener'],
            $_POST['kde'],
            (string) new \Date($_POST['datum'] ?? null),
            $_POST['visible'] ? '1' : '0',
            $_POST['lock'] ? '1' : '0',
            $id,
        );
        \Redirect::to('/admin/rozpis');
    }

    public static function duplicate($id)
    {
        \Permissions::checkError('rozpis', P_OWNED);
        \Database::query("SELECT legacy_duplicate_rozpis('?')", $id);
        \Redirect::to('/admin/rozpis');
    }

    public static function remove($id)
    {
        \Permissions::checkError('rozpis', P_OWNED);
        $data = \Database::querySingle("SELECT r_id,r_trener,r_kde,r_datum,r_visible,r_lock WHERE r_id='?'", $id);
        if (!\Permissions::check('rozpis', P_OWNED, $data['r_trener'])) {
            throw new \AuthorizationException("Máte nedostatečnou autorizaci pro tuto akci!");
        }
        \Database::query("DELETE FROM rozpis WHERE r_id='?'", $id);
        \Database::query("DELETE FROM rozpis_item WHERE ri_id_rodic='?'", $id);
        \Redirect::to('/admin/rozpis');
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

    private static function checkData(): \Form
    {
        $datum = new \Date($_POST['datum'] ?? null);

        $f = new \Form();
        $f->checkNumeric($_POST['trener'], 'Neplatný trenér');
        $f->checkDate((string) $datum, 'Neplatný formát data');
        return $f;
    }
}
