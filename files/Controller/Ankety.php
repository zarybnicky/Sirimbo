<?php
class Controller_Ankety implements Controller_Interface {
    function view($id = null) {
        if(!empty($_POST)) {
        	$ip = getIP();
        	
        	if(DBAnkety::isUniqueIP(post('id'), $ip)) {
        		DBAnkety::vote(post('id'), post('choice'), $ip);
        		notice('Váš hlas byl uložen');
        	} else {
        		notice('Z vaší IP adresy už někdo hlasoval.');
        	}
        }
        
        header_main('Ankety');
        $result = DisplayAnkety::viewAnkety(false, true);
        if(!$result)  {
        	notice('Žádné ankety');
        	return;
        }
    }
}
?>