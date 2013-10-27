<?php
require_once 'files/Controller/Admin/Akce.php' ;
class Controller_Admin_Akce_Dokumenty extends Controller_Admin_Akce
{
    function __construct() {
        Permissions::checkError('akce', P_OWNED);
    }
    function view($id = null) {
        if (!$id || !($akce = DBAkce::getSingleAkce($id)))
            $this->redirect('/admin/akce', 'Akce s takovým ID neexistuje');

        $doku = unserialize($akce["a_dokumenty"]);

        if (!empty($_POST)) {
            if (post("remove") !== null) {
                unset($doku[post('remove')]);
                $doku = array_values($doku);
                $changed = true;
            }

            if (post("add-id") && DBDokumenty::getSingleDokument(post("add-id"))) {
                $doku[] = post("add-id");
                post('add-id', 0);
                $changed = true;
            }
            if (isset($changed) && $changed) {
                DBAkce::editAkce($akce["a_id"], $akce["a_jmeno"], $akce["a_kde"], $akce["a_info"], $akce["a_od"], $akce["a_do"],
                    $akce["a_kapacita"], serialize($doku), $akce["a_lock"], $akce['a_visible']);
                $akce = DBAkce::getSingleAkce($id);
            }
        }
        $items = DBAkce::getAkceItems($id);
        $new_data = array(
                'id' => $akce['a_id'],
                'jmeno' => $akce['a_jmeno'],
                'kde' => $akce['a_kde'],
                'datum' => formatDate($akce['a_od']) .
                (($akce['a_od'] != $akce['a_do']) ? ' - ' . formatDate($akce['a_do']) : ''),
                'kapacita' => $akce['a_kapacita'],
                'volno' => $akce['a_kapacita'] - count($items),
                'showForm' => Permissions::check('akce', P_MEMBER) && !$akce['a_lock'],
                'canEdit' => Permissions::check('akce', P_OWNED),
                'info' => nl2br($akce['a_info'])
        );
        $akce = $new_data;
        $this->render("files/Admin/AkceDokumenty/Display.inc", array(
                'data' => $akce,
                'doku' => $doku
        ));
    }
}
?>