<?php
require_once 'files/Controller/Member.php';
class Controller_Member_Dokumenty extends Controller_Member
{
    function __construct() {
        Permissions::checkError('dokumenty', P_VIEW);
    }
    function view($id = null) {
        $kat = get('kat');
        if (ctype_digit($kat))
            $dokumenty = DBDokumenty::getDokumentyByKategorie($kat);
        else
            $dokumenty = DBDokumenty::getDokumenty();

        foreach ($dokumenty as &$item) {
            $new_data = array(
                    'id' => $item['d_id'],
                    'name' => $item['d_name'],
                    'fileName' => $item['d_filename'],
                    'kategorie' => Settings::$documentTypes[$item['d_kategorie']],
                    'uploadedBy' => $item['u_jmeno'], ' ',  $item['u_prijmeni']
            );
            $item = $new_data;
        }
        $this->render('files/View/Member/Dokumenty.inc', array(
                'showMenu' => !TISK,
                'data' => $dokumenty
        ));
    }
}
?>