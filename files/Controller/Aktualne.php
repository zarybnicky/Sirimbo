<?php
class Controller_Aktualne extends Controller_Abstract
{
    public function view($request) {
        $id = $request->getID();
        if (!$id || !($data = DBAktuality::getSingleAktualita($id))) {
            $this->redirect('/aktualne/posledni');
        }

        $photo = DBGalerie::getSingleFoto($data['at_foto_main']);
        $photo_uri = $photo ? $photo['gf_path'] : '';

        $this->render(
            'files/View/Main/Aktuality/Single.inc',
            array(
                'id' => $data['at_id'],
                'jmeno' => $data['at_jmeno'],
                'timestamp' => $data['at_timestamp_add'],
                'canEdit' => Permissions::check('aktuality', P_OWNED, $data['at_kdo']),
                'description' => $data['at_preview'],
                'text' => stripslashes(nl2br($data['at_text'])),
                'title_photo_uri' => '/galerie/' . $photo_uri,
                'title_photo_thumb_uri' => '/galerie/thumbnails/' . $photo_uri,
                'category' => 'Zprávy'
            )
        );
        return;
    }
    public function posledni($request) {
        $this->_aktualne("Nejnovější články");
    }
    public function videa($request) {
        $this->_aktualne('Videa', AKTUALITY_VIDEA);
    }
    public function clanky($request) {
        $this->_aktualne('Články', AKTUALITY_CLANKY);
    }
    public function kratke_zpravy($request) {
        $this->_aktualne('Krátké zprávy', AKTUALITY_KRATKE);
    }
    public function navbar() {
        return parent::navbar() .
            new Navbar(
                include SETTINGS . '/menu/novinky.php',
                false
            );
    }

    private function _aktualne($nadpis = "", $type = null) {
        $data = DBAktuality::getAktuality($type);

        if (!$data) {
            $this->render(
                'files/View/Empty.inc',
                array(
                    'nadpis' => $nadpis,
                    'notice' => 'Žádné články nejsou k dispozici.'
                )
            );
            return;
        }
        $data = array_map(
            function($item) use ($type) {
                $photo = DBGalerie::getSingleFoto($item['at_foto_main']);
                $photo_uri = $photo ? $photo['gf_path'] : '';

                return array(
                    'id'        => $item['at_id'],
                    'jmeno'     => $item['at_jmeno'],
                    'timestamp' => $item['at_timestamp_add'],
                    'canEdit'   => Permissions::check('aktuality', P_OWNED, $item['at_kdo']),
                    'preview'   => $type != AKTUALITY_VIDEA ? stripslashes(nl2br($item['at_preview'])) : '',
                    'title_photo_uri' => '/galerie/' . $photo_uri,
                    'title_photo_thumb_uri' => '/galerie/thumbnails/' . $photo_uri
                );
            },
            $data
        );
        $this->render(
            'files/View/Main/Aktuality/Overview.inc',
            array(
                'nadpis' => $nadpis,
                'data' => $data
            )
        );
    }
}
