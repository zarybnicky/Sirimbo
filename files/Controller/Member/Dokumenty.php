<?php
require_once 'files/Controller/Member.php';
class Controller_Member_Dokumenty extends Controller_Member
{
    public function __construct()
    {
        Permissions::checkError('dokumenty', P_VIEW);
    }

    public function view($request)
    {
        $kat = $request->get('kat');
        if (ctype_digit($kat)) {
            $dokumenty = DBDokumenty::getDokumentyByKategorie($kat);
        } else {
            $dokumenty = DBDokumenty::getDokumenty();
        }

        $this->render('files/View/Member/Dokumenty.inc', [
            'header' => 'Dokumenty',
            'showMenu' => !TISK,
            'kat' => $request->get('kat') ?: '',
            'data' => array_map(
                function ($item) {
                    return [
                        'id' => $item['d_id'],
                        'name' => $item['d_name'],
                        'fileName' => $item['d_filename'],
                        'kategorie' => Settings::$documentTypes[$item['d_kategorie']],
                        'uploadedBy' => $item['u_jmeno'] . ' ' . $item['u_prijmeni']
                    ];
                },
                $dokumenty
            )
        ]);
    }
}
