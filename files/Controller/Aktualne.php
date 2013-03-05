<?php
class Controller_Aktualne implements Controller_Interface  {
    function view($id = null) {
        if($id && ($data = DBAktuality::getSingleAktualita($id))) {
        	DisplayAktuality::viewClanek($data, false);
        	return;
        }
        $this->posledni(null);
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
    
    private function _aktualne($nadpis = "", $type = null) {
        header_main('Aktuálně');
        header_minor($nadpis);
        
        if($type !== null)
            $result = DisplayAktuality::viewAktuality($type);
        else
            $result = DisplayAktuality::viewAktuality();
        
		if(!$result)  {
			notice('Žádné články');
		}
    }
}
?>