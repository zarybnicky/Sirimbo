<?php
class Controller_Member_Dokumenty extends Controller_Abstract
{
    public function view($request)
    {
        Permissions::checkError('dokumenty', P_VIEW);
        $kat = $request->get('kat');
        if (ctype_digit($kat)) {
            $dokumenty = DBDokumenty::getDokumentyByKategorie($kat);
        } else {
            $dokumenty = DBDokumenty::getDokumenty();
        }

        new \RenderHelper('files/View/Member/Dokumenty.inc', [
            'header' => 'Dokumenty',
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
