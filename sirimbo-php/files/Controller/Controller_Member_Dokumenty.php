<?php
class Controller_Member_Dokumenty
{
    public function view($request)
    {
        \Permissions::checkError('dokumenty', P_VIEW);
        $kat = $_GET['kat'];
        if (ctype_digit($kat)) {
            $dokumenty = \DBDokumenty::getDokumentyByKategorie($kat);
        } else {
            $dokumenty = \DBDokumenty::getDokumenty();
        }

        new \RenderHelper('files/View/Member/Dokumenty.inc', [
            'header' => 'Dokumenty',
            'kat' => $_GET['kat'] ?: '',
            'data' => array_map(
                fn($item) => [
                    'id' => $item['d_id'],
                    'name' => $item['d_name'],
                    'fileName' => $item['d_filename'],
                    'kategorie' => \Settings::$documentTypes[$item['d_kategorie']],
                    'uploadedBy' => $item['u_jmeno'] . ' ' . $item['u_prijmeni']
                ],
                $dokumenty
            )
        ]);
    }
}
