<?php
namespace TKOlomouc\Controller;

use TKOlomouc\Model\DBGalerie;
use TKOlomouc\Utility\Request;
use TKOlomouc\Utility\Sidebar;

class Fotogalerie extends ControllerAbstract
{
    function view($id = null) {
        if ($id === null) {
            $id = 0;
            $data = DBGalerie::getSingleDir(0);
        } elseif (!($data = DBGalerie::getSingleDir($id))) {
            $this->redirect('/fotogalerie', 'Taková složka neexistuje');
        }

        $photos = DBGalerie::getFotky($id);
        if (empty($photos)) {
            $this->render('src/application/View/Empty.inc', array(
                'nadpis' => $data['gd_name'],
                'notice' => 'Žádné fotky k dispozici.'
            ));
            return;
        }
        foreach ($photos as &$row) {
            $new_row = array(
                'id' => $row['gf_id'],
                'src' => '/galerie/thumbnails/' . $row['gf_path'],
                'href' => '/' . Request::getURL() . '/foto/' . $row['gf_id']
            );
            $row = $new_row;
        }
        $this->render(
            'src/application/View/Main/Fotogalerie/Overview.inc',
            array(
                'nadpis' => $data['gd_name'],
                'photos' => $photos
            )
        );
    }
    function foto($id = null) {
        if (!$id || !($data = DBGalerie::getSingleFoto($id)))
            $this->redirect('/fotogalerie', 'Taková fotka neexistuje');

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
            'src/application/View/Main/Fotogalerie/Single.inc',
            array(
                'id'         => $id,
                'src'        => '/galerie/' . $data['gf_path'],
                'hasPrev'    => $hasPrev,
                'prevURL'    => $hasPrev ? $parent_dir[$current - 1]['gf_id'] : '',
                'returnURL'  => '/fotogalerie' . ($data['gf_id_rodic'] > 0 ? ('/' . $data['gf_id_rodic']) : ''),
                'hasNext'    => $hasNext,
                'nextURL'    => $hasNext ? $parent_dir[$current + 1]['gf_id'] : '',
            )
        );
        return;
    }
    function sidebar() {
        $dirs = DBGalerie::getDirs(true, true);

        if (empty($dirs))
            return;

        $s = new Sidebar();
        echo $s->menuHeader();
        unset($s);

        echo '<ul class="fotoroot" style="padding-top:5px;"><li>';

        $level_prev = 0;
        foreach ($dirs as $dir) {
            if ($dir['gd_hidden'] == '1')
                continue;

            if ($dir['gd_level'] > $level_prev) {
                echo '<ul class="fotolist">';
            } elseif ($dir['gd_level'] == $level_prev) {
                echo '</li>';
            } else {
                for($i = 0; $i < ($level_prev - $dir['gd_level']); $i++)
                    echo '</li></ul>';
                echo '</li>';
            }
            if ($dir['gd_id'] == 0)
                $link = "/fotogalerie";
            else
                $link = "/fotogalerie/" . $dir['gd_id'];

            if ($dir['gd_id'] == Request::getID())
                echo '<li><a class="current" href="', $link, '">';
            else
                echo '<li><a href="', $link, '">';

            echo '<img src="/style/directory.png" alt="Složka" />', $dir['gd_name'], '</a>';
            $level_prev = $dir['gd_level'];
        }
        for($i = 0; $i < $level_prev; $i++)
            echo '</li></ul>';

        echo '</li></ul>';
    }
}