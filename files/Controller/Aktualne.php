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
        $this->_aktuality("Nejnovější články");
    }
    function videa($id = null) {
        $this->_aktuality('Videa', AKTUALITY_VIDEA);
    }
    function clanky($id = null) {
        $this->_aktuality('Články', AKTUALITY_CLANKY);
    }
    function kratke_zpravy($id = null) {
        $this->_aktuality('Krátké zprávy', AKTUALITY_KRATKE);
    }
    
    private function aktualne($nadpis = "", $type = null) {
        header_main('Aktuality');
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