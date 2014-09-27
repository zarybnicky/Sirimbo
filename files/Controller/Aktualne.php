<?php
class Controller_Aktualne extends Controller_Abstract
{
    function view($id = null) {
        if (!$id || !($data = DBAktuality::getSingleAktualita($id))) {
            $this->redirect('/aktualne/posledni');
        }
        $this->render(
            'files/View/Main/Aktuality/Single.inc',
            array(
                'id'        => $data['at_id'],
                'jmeno'     => $data['at_jmeno'],
                'timestamp' => $data['at_timestamp_add'],
                'canEdit'   => Permissions::check('aktuality', P_OWNED, $data['at_kdo']),
                'text'      => stripslashes(nl2br($data['at_text']))
            )
        );
        return;
    }
    function posledni($id = null) {
        $this->_aktualne("Nejnovější články");
    }
    function videa($id = null) {
        $this->_aktualne('Videa', AKTUALITY_VIDEA);
    }
    function clanky($id = null) {
        $this->_aktualne('Články', AKTUALITY_CLANKY);
    }
    function kratke_zpravy($id = null) {
        $this->_aktualne('Krátké zprávy', AKTUALITY_KRATKE);
    }
    function sidebar() {
        $s = new Sidebar();

        echo $s->menuItem('Články', '/aktualne/clanky');
        echo $s->menuItem('Videa', '/aktualne/videa');
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
            function($item) {
                return array(
                    'id'        => $item['at_id'],
                    'jmeno'     => $item['at_jmeno'],
                    'timestamp' => $item['at_timestamp_add'],
                    'canEdit'   => Permissions::check('aktuality', P_OWNED, $item['at_kdo']),
                    'preview'   => stripslashes(nl2br($item['at_preview']))
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
