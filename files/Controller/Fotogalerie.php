<?php
class Controller_Fotogalerie extends Controller_Abstract
{
    public function view($request)
    {
        $id = $request->getID();
        if ($id === null) {
            $id = 0;
            $dir = array('gd_name' => '');
        } elseif (!($dir = DBGalerie::getSingleDir($id))) {
            $this->redirect('/fotogalerie', 'Taková složka neexistuje');
        }

        $photos = DBGalerie::getFotky($id);

        if (empty($photos)) {
            $this->render('files/View/Empty.inc', array(
                'nadpis' => $dir['gd_name'],
                'notice' => 'Žádné fotky k dispozici.'
            ));
            return;
        }

        $photos = array_map(
            function ($item) use ($request) {
                return array(
                    'id' => $item['gf_id'],
                    'src' => '/galerie/thumbnails/' . $item['gf_path'],
                    'href' => '/' . $request->getURI() . '/foto/' . $item['gf_id']
                );
            },
            $photos
        );

        $this->render(
            'files/View/Main/Fotogalerie/Overview.inc',
            array(
                'nadpis' => $dir['gd_name'],
                'photos' => $photos,
                'sidemenu' => $this->sidemenu($request)
            )
        );
    }

    public function foto($request)
    {
        $id = $request->getID();
        if (!$id || !($data = DBGalerie::getSingleFoto($id))) {
            $this->redirect('/fotogalerie', 'Taková fotka neexistuje');
        }

        $parent_dir = DBGalerie::getFotky($data['gf_id_rodic']);
        foreach ($parent_dir as $key => $foto) {
            if ($foto['gf_id'] == $id) {
                $current = $key;
                break;
            }
        }
        $hasPrev = isset($parent_dir[$current - 1]);
        $hasNext = isset($parent_dir[$current + 1]);

        $this->render(
            'files/View/Main/Fotogalerie/Single.inc',
            array(
                'id'        => $id,
                'src'       => '/galerie/' . $data['gf_path'],
                'hasPrev'   => $hasPrev,
                'hasNext'   => $hasNext,
                'prevURI'   => $hasPrev ? $parent_dir[$current - 1]['gf_id'] : '',
                'nextURI'   => $hasNext ? $parent_dir[$current + 1]['gf_id'] : '',
                'returnURI' => '/fotogalerie' . ($data['gf_id_rodic'] > 0 ? ('/' . $data['gf_id_rodic']) : ''),
                'sidemenu'   => $this->sidemenu($request)
            )
        );
    }

    public function sidemenu($request)
    {
        if (!($dirs = DBGalerie::getDirs(true, true))) {
            return;
        }

        $root = $tip = new Tag('ul', array('class' => 'fotoroot'));
        $stack = array($root);
        $level = 0;

        foreach ($dirs as $dir) {
            if ($dir['gd_hidden'] == '1') {
                continue;
            }

            if ($dir['gd_level'] > $level) {
                $newTip = new Tag('ul', array('class' => 'foto_list'));
                $tip->add($newTip);
                $stack[] = $newTip;
                $tip = $newTip;
            } elseif ($dir['gd_level'] > $level) {
                for ($i = 0; $i < ($level - $dir['gd_level']); $i++) {
                    $tip = array_pop($stack);
                }
            }
            $level = $dir['gd_level'];

            $tip->add(
                new Tag(
                    'li',
                    array(),
                    new Tag(
                        'a',
                        array(
                            'class' => ($dir['gd_id'] == $request->getID()) ? 'current' : '',
                            'href' => ($dir['gd_id'] == 0) ? '/fotogalerie' : "/fotogalerie/{$dir['gd_id']}"
                        ),
                        $dir['gd_name']
                    )
                )
            );
        }

        return (string) $root;
    }
}
