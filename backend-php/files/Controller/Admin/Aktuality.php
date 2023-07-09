<?php
namespace Olymp\Controller\Admin;

class Aktuality
{
    public static function list()
    {
        \Permissions::checkError('aktuality', P_OWNED);
        $kdo = \Permissions::check('aktuality', P_ADMIN) ? null : \Session::getUser()->getId();
        $data = \Database::queryArray(
            "SELECT * FROM aktuality LEFT JOIN galerie_foto ON gf_id=at_foto_main WHERE at_kat=1" . ($kdo ? " AND at_kdo='$kdo'" : '')
            . " ORDER BY at_timestamp_add DESC"
        );
        \Render::twig('Admin/Aktuality.twig', [
            'data' => $data
        ]);
    }

    public static function add()
    {
        \Permissions::checkError('aktuality', P_OWNED);
        \Render::twig('Admin/AktualityForm.twig', [
            'action' => 'add',
            'name' => '',
            'summary' => '',
            'text' => ''
        ]);
    }

    public static function addPost()
    {
        \Permissions::checkError('aktuality', P_OWNED);
        \Database::query(
            "INSERT INTO aktuality (at_kat,at_jmeno,at_text,at_preview) VALUES (1,'?','?','?')",
            $_POST['name'],
            $_POST['text'],
            $_POST['summary'],
        );
        $id = \Database::getInsertId();
        if ($_POST['action'] == 'save') {
            \Redirect::to('/admin/aktuality');
        } else {
            \Redirect::to("/admin/aktuality/foto/$id?notify=true");
        }
    }

    public static function edit($id)
    {
        \Permissions::checkError('aktuality', P_OWNED);
        $data = \Database::querySingle("SELECT * FROM aktuality LEFT JOIN galerie_foto ON gf_id=at_foto_main WHERE at_id='?'", $id);
        if (!$data) {
            \Message::warning('Článek s takovým ID neexistuje');
            \Redirect::to('/admin/aktuality');
        }
        \Permissions::checkError('aktuality', P_OWNED, $data['at_kdo']);
        \Render::twig('Admin/AktualityForm.twig', [
            'action' => 'edit',
            'name' => $data['at_jmeno'],
            'summary' => $data['at_preview'],
            'text' => $data['at_text'],
            'createdAt' => $data['at_timestamp_add'],
        ]);
    }

    public static function editPost($id)
    {
        \Permissions::checkError('aktuality', P_OWNED);
        $data = \Database::querySingle("SELECT * FROM aktuality LEFT JOIN galerie_foto ON gf_id=at_foto_main WHERE at_id='?'", $id);
        if (!$data) {
            \Message::warning('Článek s takovým ID neexistuje');
            \Redirect::to('/admin/aktuality');
        }
        \Permissions::checkError('aktuality', P_OWNED, $data['at_kdo']);

        if (\DateTime::createFromFormat('j. n. Y H:i', $_POST['createdAt']) === false) {
            \Message::danger('Špatný formát data "Publikováno" (D. M. RRRR HH:SS)');
            \Render::twig('Admin/AktualityForm.twig', [
                'action' => 'edit',
                'name' => $_POST['name'],
                'summary' => $_POST['summary'],
                'text' => $_POST['text'],
                'createdAt' => $_POST['createdAt'],
            ]);
            return;
        }
        \Database::query(
            "UPDATE aktuality SET at_jmeno='?',at_text='?',at_preview='?' WHERE at_id='?'",
            $_POST['name'],
            $_POST['text'],
            $_POST['summary'],
            $id,
        );
        \Redirect::to('/admin/aktuality');
    }

    public static function remove($id)
    {
        \Permissions::checkError('aktuality', P_OWNED);
        $item = \Database::querySingle("SELECT * FROM aktuality LEFT JOIN galerie_foto ON gf_id=at_foto_main WHERE at_id='?'", $id);
        \Render::twig('RemovePrompt.twig', [
            'header' => 'Správa aktualit',
            'prompt' => 'Opravdu chcete odstranit články:',
            'returnURI' => $_SERVER['HTTP_REFERER'] ?? '/admin/aktuality',
            'data' => [[
                'id' => $item['at_id'],
                'text' => $item['at_jmeno']
            ]]
        ]);
    }

    public static function removePost($id)
    {
        \Permissions::checkError('aktuality', P_OWNED);
        $data = \Database::querySingle("SELECT * FROM aktuality LEFT JOIN galerie_foto ON gf_id=at_foto_main WHERE at_id='?'", $id);
        if (!\Permissions::check('aktuality', P_OWNED, $data['at_kdo'])) {
            throw new \AuthorizationException('Máte nedostatečnou autorizaci pro tuto akci!');
        }
        \Database::query("DELETE FROM aktuality WHERE at_id='?'", $id);
        \Redirect::to('/admin/aktuality');
    }

    public static function foto($id)
    {
        \Permissions::checkError('aktuality', P_OWNED);
        $article = \Database::querySingle("SELECT * FROM aktuality LEFT JOIN galerie_foto ON gf_id=at_foto_main WHERE at_id='?'", $id);
        if (!$article) {
            \Message::warning('Takový článek neexistuje');
            \Redirect::to('/admin/aktuality');
        }
        if (!isset($_GET['dir']) && $article['at_foto']) {
            \Redirect::to('/admin/aktuality/foto/' . $id . '?dir=' . $article['at_foto']);
        }
        \Render::twig('Admin/AktualityFormFoto.twig', [
            'checked' => $article['at_foto_main'],
            'photos' => \DBGalerie::getFotky($_GET['dir'] ?? 1),
            'dir' => $_GET['dir'] ?? 1,
            'dirs' => [[
                'id' => 'none',
                'text' => '------ vyberte složku ------'
            ]] + array_map(
                fn($item) => [
                    'id' => $item['gd_id'],
                    'text' => str_repeat('&nbsp;&nbsp;', $item['gd_level'] - 1) . $item['gd_name']
                ],
                \DBGalerie::getDirs()
            ),
        ]);
    }

    public static function fotoPost($id)
    {
        \Permissions::checkError('aktuality', P_OWNED);
        $data = \Database::querySingle("SELECT * FROM aktuality LEFT JOIN galerie_foto ON gf_id=at_foto_main WHERE at_id='?'", $id);
        if (!$data) {
            \Message::warning('Takový článek neexistuje');
            \Redirect::to('/admin/aktuality');
        }
        \Database::query(
            "UPDATE aktuality
            SET at_foto='?',at_foto_main=" . ($_POST['foto'] ? "'${_POST['foto']}'" : 'NULL') . " WHERE at_id='?'",
            $_GET['dir'] ?? 1,
            $id,
        );
        \Redirect::to('/admin/aktuality');
    }
}
