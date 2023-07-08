<?php
namespace Olymp\Controller;

class Aktualne
{
    public static function list()
    {
        \Render::twig('Main/Aktuality.twig');
    }

    public static function single($id)
    {
        if (!$data = \DBAktuality::getSingleAktualita($id)) {
            \Redirect::to('/aktualne');
        }
        \Render::twig('Main/AktualitySingle.twig', [
            'data' => $data,
            'photoUri' => $data['at_foto_main'] ? (\DBGalerie::getSingleFoto($data['at_foto_main'])['gf_path'] ?? '') : '',
            'canEdit' => \Permissions::check('aktuality', P_OWNED, $data['at_kdo']),
        ]);
    }
}
