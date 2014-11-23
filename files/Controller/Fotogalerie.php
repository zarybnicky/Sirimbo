<?php
class Controller_Fotogalerie extends Controller_Abstract
{
    public function view($request) {
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
            function($item) {
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
                'sidebar' => $this->sidebar()
            )
        );
    }

    public function foto($request) {
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
                'sidebar'   => $this->sidebar()
            )
        );
    }

    public function sidebar() {
        $dirs = DBGalerie::getDirs(true, true);

        if (empty($dirs)) {
            return;
        }

        $out = '<ul class="fotoroot"><li>';

        $level_prev = 0;
        foreach ($dirs as $dir) {
            if ($dir['gd_hidden'] == '1') {
                continue;
            }

            if ($dir['gd_level'] > $level_prev) {
                $out .= '<ul class="foto_list">';
            } elseif ($dir['gd_level'] == $level_prev) {
                $out .= '</li>';
            } else {
                for($i = 0; $i < ($level_prev - $dir['gd_level']); $i++) {
                    $out .= '</li></ul>';
                }
                $out .= '</li>';
            }
            if ($dir['gd_id'] == 0) {
                $link = "/fotogalerie";
            } else {
                $link = "/fotogalerie/" . $dir['gd_id'];
            }

            if ($dir['gd_id'] == $request->getID()) {
                $out .= '<li><a class="current" href="' . $link . '">';
            } else {
                $out .= '<li><a href="' . $link . '">';
            }

            $out .= '' . $dir['gd_name'] . '</a>';
            $level_prev = $dir['gd_level'];
        }

        for ($i = 0; $i < $level_prev; $i++) {
            $out .= '</li></ul>';
        }

        $out .= '</li></ul>';
        return $out;
    }
}
