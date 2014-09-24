<?php
require_once 'files/Controller/Admin/Aktuality.php';
class Controller_Admin_Aktuality_Foto extends Controller_Admin_Aktuality
{
    public function __construct() {
        Permissions::checkError('aktuality', P_OWNED);
    }

    public function view($id = null) {
        if (!($article = DBAktuality::getSingleAktualita($id))) {
            $this->redirect('/admin/aktuality', 'Takový článek neexistuje');
        }
        if (empty($_POST) || !post('foto')) {
            if (get('dir') === null) {
                if ($article['at_foto']) {
                    $this->redirect('/admin/aktuality/foto/' . $id . '?dir=' . $article['at_foto']);
                } else {
                    get('dir', 0);
                }
            }

            if (!($dir = DBGalerie::getSingleDir(get('dir')))) {
                $this->redirect('/admin/aktuality/foto/' . $id . '?dir=0', 'Taková složka neexistuje');
                return;
            }

            $photos = array_map(
                function ($item) {
                    return array(
                        'id' => $item['gf_id'],
                        'src' => '/galerie/thumbnails/' . $item['gf_path']
                    );
                },
                DBGalerie::getFotky(get('dir'))
            );

            $dirs = DBGalerie::getDirs(true, true);
            foreach ($dirs as $item) {
                $dirs_out[$item['gd_id']] = str_repeat("&nbsp;&nbsp;", $item['gd_level'] - 1) . $item['gd_name'];
            }

            $this->render(
                'files/Admin/AktualityFoto/Form.inc',
                array(
                    'nadpis' => $dir['gd_name'],
                    'photos' => $photos,
                    'dirs' => $dirs_out,
                    'checked' => $article['at_foto_main']
                )
            );
            return;
        }
        if (get('dir') === null) {
            get('dir', 0);
        }

        DBAktuality::editAktualita(
            $id, $article['at_kat'], $article['at_jmeno'],
            $article['at_text'], $article['at_preview'], get('dir'), post('foto')
        );
        $this->redirect('/admin/aktuality', 'Článek upraven.');
    }
}
