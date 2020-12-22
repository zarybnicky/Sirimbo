<?php
namespace Olymp\Controller;

class Fotogalerie
{
    public static function root()
    {
        static::directory(0);
    }

    public static function directory($id)
    {
        if (!($dir = \DBGalerie::getSingleDir($id))) {
            \Message::warning('Taková složka neexistuje');
            \Redirect::to('/fotogalerie');
        }

        $photos = \DBGalerie::getFotky($id);
        if (!$photos) {
            return new \RenderHelper('files/View/Empty.inc', [
                'nadpis' => $dir['gd_name'],
                'notice' => 'Žádné fotky k dispozici.'
            ]);
        }

        new \RenderHelper('files/View/Main/Fotogalerie/Overview.inc', [
            'nadpis' => $dir['gd_name'],
            'sidemenu' => static::sidemenu($id),
            'photos' => array_map(
                fn($item) => [
                    'id' => $item['gf_id'],
                    'src' => '/galerie/thumbnails/' . $item['gf_path'],
                    'href' => explode('?', $_SERVER['REQUEST_URI'])[0] . '/foto/' . $item['gf_id']
                ],
                $photos
            ),
        ]);
    }

    public static function singleWithDir($dirId, $id)
    {
        return self::single($id);
    }

    public static function single($id)
    {
        if (!$data = \DBGalerie::getSingleFoto($id)) {
            \Message::warning('Taková fotka neexistuje');
            \Redirect::to('/fotogalerie');
        }

        $parent_dir = \DBGalerie::getFotky($data['gf_id_rodic']);
        $current = 0;
        foreach ($parent_dir as $key => $foto) {
            if ($foto['gf_id'] == $id) {
                $current = $key;
                break;
            }
        }
        $hasPrev = isset($parent_dir[$current - 1]);
        $hasNext = isset($parent_dir[$current + 1]);

        new \RenderHelper('files/View/Main/Fotogalerie/Single.inc', [
            'id'        => $id,
            'src'       => '/galerie/' . $data['gf_path'],
            'hasPrev'   => $hasPrev,
            'hasNext'   => $hasNext,
            'prevURI'   => $hasPrev ? $parent_dir[$current - 1]['gf_id'] : '',
            'nextURI'   => $hasNext ? $parent_dir[$current + 1]['gf_id'] : '',
            'returnURI' => '/fotogalerie' . ($data['gf_id_rodic'] > 0 ? ('/' . $data['gf_id_rodic']) : ''),
            'sidemenu'  => static::sidemenu($data['gf_id_rodic'])
        ]);
    }

    public static function sidemenu($dirId)
    {
        if (!($dirs = \DBGalerie::getDirs(true, true))) {
            return '';
        }

        $root = $tip = new \Tag('ul', ['class' => 'fotoroot']);
        $stack = [];
        $level = 0;

        foreach ($dirs as $dir) {
            if ($dir['gd_hidden'] == '1') {
                continue;
            }

            if ($dir['gd_level'] > $level) {
                $stack[] = $tip;
                $newTip = new \Tag('ul', ['class' => 'foto_list']);
                $tip->add($newTip);
                $tip = $newTip;
            } elseif ($dir['gd_level'] < $level) {
                for ($i = 0; $i < ($level - $dir['gd_level']); $i++) {
                    /** @var \Tag */
                    $tip = array_pop($stack);
                }
            }
            $level = $dir['gd_level'];

            $cls = ($dir['gd_id'] == $dirId) ? 'current' : '';
            $href = ($dir['gd_id'] == 0) ? '/fotogalerie' : "/fotogalerie/{$dir['gd_id']}";
            $tip->add("<li><a class='$cls' href='$href'>{$dir['gd_name']}</a></li>");
        }

        return (string) $root;
    }
}
