<?php
class Controller_Aktualne extends Controller_Abstract
{
    public function view($request)
    {
        if (!($id = $request->getID())) {
            $this->redirect('/aktualne/posledni');
        }
        if (!($data = DBAktuality::getSingleAktualita($id))) {
            $this->redirect('/aktualne/posledni');
        }

        $photo = DBGalerie::getSingleFoto($data['at_foto_main']);
        $photo_uri = $photo ? $photo['gf_path'] : '';

        $this->render(
            'files/View/Main/Aktuality/Single.inc',
            [
                'id' => $data['at_id'],
                'jmeno' => $data['at_jmeno'],
                'timestamp' => $data['at_timestamp_add'],
                'canEdit' => Permissions::check('aktuality', P_OWNED, $data['at_kdo']),
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

    public function posledni($request)
    {
        $this->_aktualne("Nejnovější články");
    }

    public function clanky($request)
    {
        $this->_aktualne('Články', 1);
    }

    private function _aktualne($nadpis = "", $type = null)
    {
        $data = DBAktuality::getAktuality($type);

        if (!$data) {
            $this->render('files/View/Empty.inc', [
                'header' => $nadpis,
                'notice' => 'Žádné články nejsou k dispozici.'
            ]);
            return;
        }
        $data = array_map(
            function($item) {
                $photo = DBGalerie::getSingleFoto($item['at_foto_main']);
                $photo_uri = $photo ? $photo['gf_path'] : '';

                return [
                    'id'        => $item['at_id'],
                    'jmeno'     => $item['at_jmeno'],
                    'timestamp' => $item['at_timestamp_add'],
                    'canEdit'   => Permissions::check('aktuality', P_OWNED, $item['at_kdo']),
                    'preview'   => $item['at_preview'],
                    'title_photo_uri' => '/galerie/' . $photo_uri,
                    'title_photo_thumb_uri' => '/galerie/thumbnails/' . $photo_uri
                ];
            },
            $data
        );
        $this->render('files/View/Main/Aktuality/Overview.inc', [
            'header' => $nadpis,
            'data' => $data
        ]);
    }
}
