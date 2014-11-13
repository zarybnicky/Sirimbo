<?php
require_once 'files/Controller/Admin/Akce.php' ;
class Controller_Admin_Akce_Dokumenty extends Controller_Admin_Akce
{
    function __construct()
    {
        Permissions::checkError('akce', P_OWNED);
    }
    function view($id = null)
    {
        if (!$id || !($akce = DBAkce::getSingleAkce($id))) {
            $this->redirect('/admin/akce', 'Akce s takovým ID neexistuje');
        }
        $documents = unserialize($akce["a_dokumenty"]);

        if (!empty($_POST)) {
            if (post("remove") !== null) {
                unset($documents[array_search(post('remove'), $documents)]);
                $documents = array_values($documents);
                $changed = true;
            }
            if (post("add-id") && DBDokumenty::getSingleDokument(post("add-id"))) {
                $documents[] = post("add-id");
                post('add-id', 0);
                $changed = true;
            }
            if (isset($changed) && $changed) {
                DBAkce::editAkce(
                    $akce["a_id"], $akce["a_jmeno"], $akce["a_kde"],
                    $akce["a_info"], $akce["a_od"], $akce["a_do"],
                    $akce["a_kapacita"], serialize($documents),
                    $akce["a_lock"], $akce['a_visible']
                );
                $akce = DBAkce::getSingleAkce($id);
            }
        }
        
        $booked = count(DBAkce::getAkceItems($id));
        $akce = array(
            'id' => $akce['a_id'],
            'jmeno' => $akce['a_jmeno'],
            'kde' => $akce['a_kde'],
            'datum' => formatDate($akce['a_od'])
            . (($akce['a_od'] != $akce['a_do'])
               ? ' - ' . formatDate($akce['a_do']) : ''),
            'kapacita' => $akce['a_kapacita'],
            'volno' => $akce['a_kapacita'] - $booked,
            'showForm' => Permissions::check('akce', P_MEMBER) && !$akce['a_lock'],
            'canEdit' => Permissions::check('akce', P_OWNED),
            'info' => nl2br($akce['a_info'])
        );
        
        $documents = array_map(
            function($item) {
                return array(
                    'name' => $item['d_name'],
                    'category' => Settings::$documentTypes[$item['d_kategorie']],
                    'removeButton' =>
                        '<button type="submit" name="remove" ' .
                        'value="' . $item['d_id'] . '">Odstranit</button>'
                );
            },
            DBDokumenty::getMultipleById($documents)
        );

        $allDocuments = array();
        foreach (array(2, 3, 0) as $category) {
            foreach (DBDokumenty::getDokumentyByKategorie($category) as $item) {
                $allDocuments[$item['d_id']] =
                    Settings::$documentTypes[$item['d_kategorie']] . ' - ' .
                    $item['d_name'];
            }
        }
        $documentSelect = $this->select()
                               ->name('add-id')
                               ->option('', '----------')
                               ->options($allDocuments);
        $documents[] = array(
            'name' => (string) $documentSelect,
            'category' => '<button type="submit" name="add" value="add">Přidat</button>',
            'removeButton' => ''
        );
        $this->render(
            'files/Admin/AkceDokumenty/Display.inc',
            array(
                'data' => $akce,
                'documents' => $documents
            )
        );
    }
}
