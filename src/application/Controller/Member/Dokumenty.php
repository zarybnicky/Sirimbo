<?php
namespace TKOlomouc\Controller\Member;

use TKOlomouc\Controller\Member;
use TKOlomouc\Settings;
use TKOlomouc\Utility\Permissions;
use TKOlomouc\Model\DBDokumenty;

class Dokumenty extends Member
{
    public function __construct()
    {
        Permissions::checkError('dokumenty', P_VIEW);
    }

    public function view($id = null)
    {
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
        $this->render(
            'src/application/View/Member/Dokumenty.inc',
            array(
                'showMenu' => !TISK,
                'data' => $dokumenty
            )
        );
    }
}
