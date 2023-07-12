<?php
namespace Olymp\Controller;

class Aktualne
{
    public static function single($id)
    {
        $data = \Database::querySingle("SELECT * FROM aktuality LEFT JOIN galerie_foto ON gf_id=at_foto_main WHERE at_id='?'", $id);
        $foto = \Database::querySingle("SELECT * FROM galerie_foto WHERE gf_id='?'", $data['at_foto_main'] ?? '');
        if (!$data) {
            \Redirect::to('/aktualne');
        }
        \Render::twig('Main/AktualitySingle.twig', [
            'data' => $data,
            'photoUri' => $foto ? ($foto['gf_path'] ?? '') : '',
            'canEdit' => \Permissions::check('aktuality', P_OWNED, $data['at_kdo']),
        ]);
    }
}
