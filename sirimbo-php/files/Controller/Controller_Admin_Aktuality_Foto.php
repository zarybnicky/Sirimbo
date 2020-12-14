<?php
class Controller_Admin_Aktuality_Foto
{
    public function view($request)
    {
        \Permissions::checkError('aktuality', P_OWNED);
        $id = $request->getId();
        if (!($article = DBAktuality::getSingleAktualita($id))) {
            new \MessageHelper('warning', 'Takový článek neexistuje');
            new \RedirectHelper('/admin/aktuality');
        }
        if (!$_POST['foto']) {
            if ($_GET['dir'] === null) {
                if ($article['at_foto']) {
                    new \RedirectHelper('/admin/aktuality/foto/' . $id . '?dir=' . $article['at_foto']);
                } else {
                    $_GET['dir'] = 0;
                }
            }

            if (!\DBGalerie::getSingleDir($_GET['dir'])) {
                new \MessageHelper('warning', 'Taková složka neexistuje');
                return new \RedirectHelper('/admin/aktuality/foto/' . $id . '?dir=0');
            }

            $photos = array_map(fn($item) => [
                'id' => $item['gf_id'],
                'name' => $item['gf_name'],
                'src' => '/galerie/thumbnails/' . $item['gf_path']
            ], \DBGalerie::getFotky($_GET['dir']));

            $dirs = \DBGalerie::getDirs(true, true);
            $dirs_out = [];
            foreach ($dirs as $item) {
                $dirs_out[$item['gd_id']] = str_repeat("&nbsp;&nbsp;", $item['gd_level'] - 1) . $item['gd_name'];
            }

            return new \RenderHelper('files/View/Admin/Aktuality/FormFoto.inc', [
                'header' => 'Správa článků',
                'photos' => $photos,
                'dir' => $_GET['dir'] ?: '',
                'dirs' => $dirs_out,
                'checked' => $article['at_foto_main']
            ]);
        }
        if ($_GET['dir'] === null) {
            $_GET['dir'] = 0;
        }

        DBAktuality::editAktualita(
            $id,
            $article['at_kat'],
            $article['at_jmeno'],
            $article['at_text'],
            $article['at_preview'],
            $_GET['dir'],
            $_POST['foto'],
            $article['at_timestamp_add']
        );
        new \RedirectHelper('/admin/aktuality');
    }
}
