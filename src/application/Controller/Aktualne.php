<?php
class Controller_Aktualne extends Controller_Abstract
{
    function view($id = null) {
        if ($id && ($data = DBAktuality::getSingleAktualita($id))) {
            $this->render(
                'src/application/View/Main/Aktuality/Single.inc',
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
        $this->redirect('/aktualne/posledni');
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

        echo $s->menuHeader();
        echo $s->menuItem('Nejnovější články',    '/aktualne/posledni');
        echo $s->menuItem('Videa',                '/aktualne/videa');
        echo $s->menuItem('Články',               '/aktualne/clanky');
        echo $s->menuItem('Krátké zprávy',        '/aktualne/kratke-zpravy');

        echo $s->commonItems();
    }
    private function _aktualne($nadpis = "", $type = null) {
        $data = DBAktuality::getAktuality($type);

        if (!$data) {
            $this->render(
                'src/application/View/Empty.inc',
                array(
                    'nadpis' => $nadpis,
                    'notice' => 'Žádné články nejsou k dispozici.'
                )
            );
            return;
        }
        foreach ($data as &$row) {
            $new_row = array(
                'id'         => $row['at_id'],
                'jmeno'      => $row['at_jmeno'],
                'timestamp'  => $row['at_timestamp_add'],
                'canEdit'    => Permissions::check('aktuality', P_OWNED, $row['at_kdo']),
                'preview'    => stripslashes(nl2br($row['at_preview']))
            );
            $row = $new_row;
        }
        $this->render(
            'src/application/View/Main/Aktuality/Overview.inc',
            array(
                'nadpis' => $nadpis,
                'data' => $data
            )
        );
    }
}
?>