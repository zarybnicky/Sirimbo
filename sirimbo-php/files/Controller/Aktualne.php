<?php
namespace Olymp\Controller;

class Aktualne
{
    public static function list()
    {
        $data = \DBAktuality::getAktuality();
        if (!$data) {
            \Render::twig('Empty.twig', [
                'header' => "Články",
                'notice' => 'Žádné články nejsou k dispozici.'
            ]);
            return;
        }
        $data = array_map(
            function ($item) {
                $photo_uri = \DBGalerie::getSingleFoto($item['at_foto_main'])['gf_path'] ?? '';
                return [
                    'id'        => $item['at_id'],
                    'jmeno'     => $item['at_jmeno'],
                    'timestamp' => $item['at_timestamp_add'],
                    'canEdit'   => \Permissions::check('aktuality', P_OWNED, $item['at_kdo']),
                    'preview'   => $item['at_preview'],
                    'title_photo_uri' => '/galerie/' . $photo_uri,
                    'title_photo_thumb_uri' => '/galerie/thumbnails/' . $photo_uri
                ];
            },
            $data
        );
        \Render::page('files/View/Main/Aktuality/Overview.inc', [
            'header' => "Články",
            'data' => $data
        ]);
    }

    public static function single($id)
    {
        if (!($data = \DBAktuality::getSingleAktualita($id))) {
            \Redirect::to('/aktualne');
        }

        $photo = \DBGalerie::getSingleFoto($data['at_foto_main']);
        $photo_uri = $photo ? $photo['gf_path'] : '';

        \Render::page(
            'files/View/Main/Aktuality/Single.inc',
            [
                'id' => $data['at_id'],
                'jmeno' => $data['at_jmeno'],
                'timestamp' => $data['at_timestamp_add'],
                'canEdit' => \Permissions::check('aktuality', P_OWNED, $data['at_kdo']),
                'description' => $data['at_preview'],
                'text' => $data['at_text'],
                'title_photo_uri' => '/galerie/' . $photo_uri,
                'title_photo_thumb_uri' => '/galerie/thumbnails/' . $photo_uri,
                'meta' => [
                    ['property' => 'og:title', 'content' => $data['at_jmeno']],
                    ['property' => 'og:type', 'content' => 'article'],
                    ['property' => 'og:url', 'content' => 'http://tkolymp.cz/aktualne/' . $data['at_id']],
                    ['property' => 'og:image', 'content' => 'http://tkolymp.cz/galerie/thumbnails/' . $photo_uri],
                    ['property' => 'og:site_name', 'TK Olymp'],
                    ['property' => 'og:description', $data['at_preview']]
                ],
                'header' => $data['at_jmeno']
            ]
        );
    }
}
