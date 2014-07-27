<?php
require_once 'files/Controller/Admin/Aktuality.php';
class Controller_Admin_Aktuality_Foto extends Controller_Admin_Aktuality
{
    function __construct() {
        Permissions::checkError('aktuality', P_OWNED);
    }
    function view($id = null) {
        if (!($article = DBAktuality::getSingleAktualita($id))) {
            $this->redirect('/admin/aktuality', 'Takový článek neexistuje');
        }

        if (empty($_POST) || !post('foto')) {
            if (get('dir_id') === null) {
                if ($article['at_foto'] != 0) {
                    $this->redirect('/admin/aktuality/foto/' . $id . '?dir_id=' . $article['at_foto']);
                } else {
                    get('dir_id', 0);
                    $dir = array('gd_name' => 'Hlavní');
                }
            } elseif (!($dir = DBGalerie::getSingleDir(get('dir_id')))) {
                $this->redirect('/admin/aktuality/foto/' . $id . '?dir_id=0', 'Taková složka neexistuje');
                return;
            }

            $photos = DBGalerie::getFotky(get('dir_id'));

            foreach ($photos as &$row) {
                $new_row = array(
                    'id' => $row['gf_id'],
                    'src' => '/galerie/thumbnails/' . $row['gf_path']
                );
                $row = $new_row;
            }

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

        DBAktuality::editAktualita(
            $id, $article['at_kat'], $article['at_jmeno'],
            $article['at_text'], $article['at_preview'], get('dir_id'), post('foto')
        );
        $n = new Novinky(User::getUserID());
        if (get('notify')) {
            switch($article['at_kat']) {
            	case AKTUALITY_VIDEA:
            	    $n->video()->add('/aktualne/' . $id, $article['at_jmeno']);
            	    break;
            	case AKTUALITY_CLANKY:
            	    $n->clanek()->add('/aktualne/' . $id, $article['at_jmeno']);
            	    break;
            }
        }

        $this->redirect('/admin/aktuality', 'Článek upraven.');
    }
}
?>