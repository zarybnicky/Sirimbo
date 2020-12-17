<?php
namespace Olymp\Controller\Admin;

class Aktuality
{
    public static function list()
    {
        \Permissions::checkError('aktuality', P_OWNED);
        $data = array_map(
            fn($item) => [
                'name' => $item['at_jmeno'],
                'added' => \Format::timestamp($item['at_timestamp_add']),
                'links' => (
                    '<a href="/admin/aktuality/edit/' . $item['at_id'] . '">obecné</a>, ' .
                    '<a href="/admin/aktuality/foto/' . $item['at_id'] . '">galerie</a>'
                ),
                'buttons' => \Buttons::delete('/admin/aktuality/remove/' . $item['at_id'])
            ],
            \Permissions::check('aktuality', P_ADMIN)
            ? \DBAktuality::getAktuality(1)
            : \DBAktuality::getAktuality(1, \Session::getUserID())
        );
        new \RenderHelper('files/View/Admin/Aktuality/Overview.inc', [
            'header' => 'Správa aktualit',
            'data' => $data,
        ]);
    }

    public static function add()
    {
        \Permissions::checkError('aktuality', P_OWNED);
        return new \RenderHelper('files/View/Admin/Aktuality/Form.inc', [
            'header' => 'Správa aktualit',
            'subheader' => 'Přidat článek',
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
            \Session::getUserID(),
            1,
            $_POST['name'],
            $_POST['text'],
            $_POST['summary'],
            '0',
            '0'
        );
        if ($_POST['action'] == 'save') {
            new \RedirectHelper('/admin/aktuality');
        } else {
            new \RedirectHelper("/admin/aktuality/foto/$id?notify=true");
        }
    }

    public static function edit($id)
    {
        \Permissions::checkError('aktuality', P_OWNED);
        if (!$data = \DBAktuality::getSingleAktualita($id)) {
            new \MessageHelper('warning', 'Článek s takovým ID neexistuje');
            new \RedirectHelper('/admin/aktuality');
        }
        \Permissions::checkError('aktuality', P_OWNED, $data['at_kdo']);
        return new \RenderHelper('files/View/Admin/Aktuality/Form.inc', [
            'header' => 'Správa aktualit',
            'subheader' => 'Upravit článek',
            'action' => 'edit',
            'name' => $data['at_jmeno'],
            'summary' => $data['at_preview'],
            'text' => $data['at_text'],
            'createdAt' => \Format::timestamp($data['at_timestamp_add']),
        ]);
    }

    public static function editPost($id)
    {
        \Permissions::checkError('aktuality', P_OWNED);
        if (!$data = \DBAktuality::getSingleAktualita($id)) {
            new \MessageHelper('warning', 'Článek s takovým ID neexistuje');
            new \RedirectHelper('/admin/aktuality');
        }
        \Permissions::checkError('aktuality', P_OWNED, $data['at_kdo']);

        if (\DateTime::createFromFormat('j. n. Y H:i', $_POST['createdAt']) === false) {
            new \MessageHelper('danger', 'Špatný formát data "Publikováno" (D. M. RRRR HH:SS)');
            return new \RenderHelper('files/View/Admin/Aktuality/Form.inc', [
                'header' => 'Správa aktualit',
                'subheader' => 'Upravit článek',
                'action' => 'edit',
                'name' => $_POST['name'],
                'summary' => $_POST['summary'],
                'text' => $_POST['text'],
                'createdAt' => $_POST['createdAt'],
            ]);
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
        new \RedirectHelper('/admin/aktuality');
    }

    public static function remove($id)
    {
        \Permissions::checkError('aktuality', P_OWNED);
        $item = \DBAktuality::getSingleAktualita($id);
        new \RenderHelper('files/View/Admin/RemovePrompt.inc', [
            'header' => 'Správa aktualit',
            'prompt' => 'Opravdu chcete odstranit články:',
            'returnURI' => $_SERVER['HTTP_REFERER'] ?: '/admin/aktuality',
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
        new \RedirectHelper('/admin/aktuality');
    }

    public static function foto($id)
    {
        \Permissions::checkError('aktuality', P_OWNED);
        if (!($article = \DBAktuality::getSingleAktualita($id))) {
            new \MessageHelper('warning', 'Takový článek neexistuje');
            new \RedirectHelper('/admin/aktuality');
        }
        if (!isset($_GET['dir']) && $article['at_foto']) {
            return new \RedirectHelper('/admin/aktuality/foto/' . $id . '?dir=' . $article['at_foto']);
        }
        if (!\DBGalerie::getSingleDir($_GET['dir'] ?? 0)) {
            new \MessageHelper('warning', 'Taková složka neexistuje');
            return new \RedirectHelper('/admin/aktuality/foto/' . $id . '?dir=0');
        }
        $photos = array_map(
            fn($item) => [
                'id' => $item['gf_id'],
                'name' => $item['gf_name'],
                'src' => '/galerie/thumbnails/' . $item['gf_path']
            ],
            \DBGalerie::getFotky($_GET['dir'] ?? 0)
        );
        $dirs = [];
        foreach (\DBGalerie::getDirs(true, true) as $item) {
            $dirs[$item['gd_id']] = str_repeat("&nbsp;&nbsp;", $item['gd_level'] - 1) . $item['gd_name'];
        }

        return new \RenderHelper('files/View/Admin/Aktuality/FormFoto.inc', [
            'header' => 'Správa článků',
            'photos' => $photos,
            'dir' => $_GET['dir'] ?? 0,
            'dirs' => $dirs,
            'checked' => $article['at_foto_main']
        ]);
    }

    public static function fotoPost($id)
    {
        \Permissions::checkError('aktuality', P_OWNED);
        if (!($article = \DBAktuality::getSingleAktualita($id))) {
            new \MessageHelper('warning', 'Takový článek neexistuje');
            new \RedirectHelper('/admin/aktuality');
        }
        \DBAktuality::editAktualita(
            $id,
            $article['at_kat'],
            $article['at_jmeno'],
            $article['at_text'],
            $article['at_preview'],
            $_GET['dir'] ?? 0,
            $_POST['foto'],
            $article['at_timestamp_add']
        );
        new \RedirectHelper('/admin/aktuality');
    }
}
