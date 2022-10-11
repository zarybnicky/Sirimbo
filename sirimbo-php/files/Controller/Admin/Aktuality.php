<?php
namespace Olymp\Controller\Admin;

class Aktuality
{
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
        $id = \DBAktuality::addAktualita(
            \Session::getUser()->getId(),
            1,
            $_POST['name'],
            $_POST['text'],
            $_POST['summary'],
            '0',
            '0'
        );
        if ($_POST['action'] == 'save') {
            \Redirect::to('/admin/aktuality');
        } else {
            \Redirect::to("/admin/aktuality/foto/$id?notify=true");
        }
    }

    public static function edit($id)
    {
        \Permissions::checkError('aktuality', P_OWNED);
        if (!$data = \DBAktuality::getSingleAktualita($id)) {
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
        if (!$data = \DBAktuality::getSingleAktualita($id)) {
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
        \DBAktuality::editAktualita(
            $id,
            1,
            $_POST['name'],
            $_POST['text'],
            $_POST['summary'],
            $data['at_foto'],
            $data['at_foto_main'],
            \DateTime::createFromFormat('j. n. Y H:i', $_POST['createdAt'])->format('Y-m-d H:i:s'),
        );
        \Redirect::to('/admin/aktuality');
    }

    public static function remove($id)
    {
        \Permissions::checkError('aktuality', P_OWNED);
        $item = \DBAktuality::getSingleAktualita($id);
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
        $data = \DBAktuality::getSingleAktualita($id);
        if (!\Permissions::check('aktuality', P_OWNED, $data['at_kdo'])) {
            throw new \AuthorizationException('Máte nedostatečnou autorizaci pro tuto akci!');
        }
        \DBAktuality::removeAktualita($id);
        \Redirect::to('/admin/aktuality');
    }

    public static function foto($id)
    {
        \Permissions::checkError('aktuality', P_OWNED);
        if (!($article = \DBAktuality::getSingleAktualita($id))) {
            \Message::warning('Takový článek neexistuje');
            \Redirect::to('/admin/aktuality');
        }
        if (!isset($_GET['dir']) && $article['at_foto']) {
            \Redirect::to('/admin/aktuality/foto/' . $id . '?dir=' . $article['at_foto']);
        }
        if (!\DBGalerie::getSingleDir($_GET['dir'] ?? 0)) {
            \Message::warning('Taková složka neexistuje');
            \Redirect::to('/admin/aktuality/foto/' . $id . '?dir=1');
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
        if (!($article = \DBAktuality::getSingleAktualita($id))) {
            \Message::warning('Takový článek neexistuje');
            \Redirect::to('/admin/aktuality');
        }
        \DBAktuality::editAktualita(
            $id,
            $article['at_kat'],
            $article['at_jmeno'],
            $article['at_text'],
            $article['at_preview'],
            $_GET['dir'] ?? 1,
            $_POST['foto'],
            $article['at_timestamp_add']
        );
        \Redirect::to('/admin/aktuality');
    }
}
