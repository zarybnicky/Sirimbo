<?php
namespace Olymp\Controller;

class Home
{
    public static function get()
    {
        $articles = \Database::queryArray(
            "SELECT * FROM aktuality LEFT JOIN galerie_foto ON gf_id=at_foto_main ORDER BY at_timestamp_add DESC limit 6"
        );

        $highlights = array_for(array_slice($articles, 0, 3), fn($val) => [
            'uri'  => '/aktualne/' . $val['at_id'],
            'name' => $val['at_jmeno'],
            'description' => $val['at_preview'],
            'title_photo_uri' => $val['at_foto_main'] ? '/galerie/' . $val['gf_path'] : ''
        ]);
        array_unshift($highlights, [
            'uri' => '/prijdtancit',
            'name' => "Přijď tančit!",
            "description" => "Nečekejte, až vaše děti vyrostou. Vrcholoví sportovci začínají již v dětském věku.",
            "title_photo_uri" => "https://tkolymp.cz/galerie/clanky/TKOLYMP-nabor-FB-uvod-820x462.jpg",
        ]);
        $moreArticles = array_for(array_slice($articles, 3, 2), fn($val) => [
            'uri'  => '/aktualne/' . $val['at_id'],
            'name' => $val['at_jmeno'],
            'description' => $val['at_preview'],
            'title_photo_uri' => $val['at_foto_main'] ? '/galerie/' . $val['gf_path'] : ''
        ]);

        \Render::twig('Main/Home.twig', [
            'highlights' => $highlights,
            'moreArticles' => $moreArticles
        ]);
    }
}
