<?php
class Controller_Fotogalerie extends Controller_Abstract
{
    public function view($request)
    {
        $id = $request->getID();
        if ($id === null) {
            $id = 0;
            $dir = ['gd_name' => ''];
        } elseif (!($dir = DBGalerie::getSingleDir($id))) {
            $this->redirect()->warning('Taková složka neexistuje');
            $this->redirect('/fotogalerie');
        }

        $photos = DBGalerie::getFotky($id);

        if (empty($photos)) {
            return new \RenderHelper('files/View/Empty.inc', [
                'nadpis' => $dir['gd_name'],
                'notice' => 'Žádné fotky k dispozici.'
            ]);
        }

        $photos = array_map(
            function ($item) use ($request) {
                return [
                    'id' => $item['gf_id'],
                    'src' => '/galerie/thumbnails/' . $item['gf_path'],
                    'href' => '/' . $request->getURI() . '/foto/' . $item['gf_id']
                ];
            },
            $photos
        );

        new \RenderHelper(
            'files/View/Main/Fotogalerie/Overview.inc',
            [
                'nadpis' => $dir['gd_name'],
                'photos' => $photos,
                'sidemenu' => $this->sidemenu($request)
            ]
        );
    }

    public function foto($request)
    {
        if (!$id = $request->getID()) {
            $this->redirect()->warning('Taková fotka neexistuje');
            $this->redirect('/fotogalerie');
        }
        if (!$data = DBGalerie::getSingleFoto($id)) {
            $this->redirect()->warning('Taková fotka neexistuje');
            $this->redirect('/fotogalerie');
        }

        $parent_dir = DBGalerie::getFotky($data['gf_id_rodic']);
        $current = 0;
        foreach ($parent_dir as $key => $foto) {
            if ($foto['gf_id'] == $id) {
                $current = $key;
                break;
            }
        }
        $hasPrev = isset($parent_dir[$current - 1]);
        $hasNext = isset($parent_dir[$current + 1]);

        new \RenderHelper(
            'files/View/Main/Fotogalerie/Single.inc',
            [
                'id'        => $id,
                'src'       => '/galerie/' . $data['gf_path'],
                'hasPrev'   => $hasPrev,
                'hasNext'   => $hasNext,
                'prevURI'   => $hasPrev ? $parent_dir[$current - 1]['gf_id'] : '',
                'nextURI'   => $hasNext ? $parent_dir[$current + 1]['gf_id'] : '',
                'returnURI' => '/fotogalerie' . ($data['gf_id_rodic'] > 0 ? ('/' . $data['gf_id_rodic']) : ''),
                'sidemenu'   => $this->sidemenu($request)
            ]
        );
    }

    public function sidemenu($request)
    {
        if (!($dirs = DBGalerie::getDirs(true, true))) {
            return '';
        }

        $root = $tip = new Tag('ul', ['class' => 'fotoroot']);
        $stack = [];
        $level = 0;

        foreach ($dirs as $dir) {
            if ($dir['gd_hidden'] == '1') {
                continue;
            }

            if ($dir['gd_level'] > $level) {
                $stack[] = $tip;
                $newTip = new Tag('ul', ['class' => 'foto_list']);
                $tip->add($newTip);
                $tip = $newTip;
            } elseif ($dir['gd_level'] < $level) {
                for ($i = 0; $i < ($level - $dir['gd_level']); $i++) {
                    /** @var Tag */
                    $tip = array_pop($stack);
                }
            }
            $level = $dir['gd_level'];

            $tip->add(
                new Tag(
                    'li',
                    [],
                    new Tag(
                        'a',
                        [
                            'class' => ($dir['gd_id'] == $request->getID()) ? 'current' : '',
                            'href' => ($dir['gd_id'] == 0) ? '/fotogalerie' : "/fotogalerie/{$dir['gd_id']}"
                        ],
                        $dir['gd_name']
                    )
                )
            );
        }

        return (string) $root;
    }
}
