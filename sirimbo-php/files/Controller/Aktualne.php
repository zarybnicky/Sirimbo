<?php
namespace Olymp\Controller;

class Aktualne
{
    public static function list()
    {
        \Render::twig('Main/Aktuality.twig', [
            'data' => array_map(
                fn($item) => $item + [
                    'canEdit' => \Permissions::check('aktuality', P_OWNED, $item['at_kdo']),
                    'photo_uri' => \DBGalerie::getSingleFoto($item['at_foto_main'])['gf_path'] ?? '',
                ],
                \DBAktuality::getAktuality()
            ),
        ]);
    }

    public static function single($id)
    {
        if (!$data = \DBAktuality::getSingleAktualita($id)) {
            \Redirect::to('/aktualne');
        }
        \Render::twig('Main/AktualitySingle.twig', [
            'data' => $data,
            'photoUri' => \DBGalerie::getSingleFoto($data['at_foto_main'])['gf_path'] ?? '',
            'canEdit' => \Permissions::check('aktuality', P_OWNED, $data['at_kdo']),
        ]);
    }
}
