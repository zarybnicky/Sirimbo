<?php
class Controller_Member_Rozpis implements Controller_Interface {
	function __construct() {
		Permissions::checkError('rozpis', P_VIEW);
	}
    function view($id = null) {
        $rozpis = DBRozpis::getRozpis();
        if(empty($rozpis)) {
        	notice('Žádná nabídka k dispozici');
        	return;
        }
        
        if(!empty($_POST)) {
            $data = DBRozpis::getSingleRozpis(post('ri_id'));
            
            $f = new Form();
            $f->checkBool(!$data['r_lock'], 'Tento rozpis je uzamčený', '');
            $f->checkInArray(post('action'), array('signup', 'signout'), 'Špatná akce', '');
            
            if($f->isValid()) {
            	$lesson = DBRozpis::getRozpisItemLesson(post('ri_id'));
                
        		if(post('action') == 'signup') {
                    $partnerka = DBUser::getUserData(User::getPartnerID());
                    /*if(!User::getZaplaceno() || !(strcmp($partnerka['up_plati_do'], date('Y-m-d')) >= 0)) {
                        notice('Buď vy nebo váš partner(ka) nemáte zaplacené členské příspěvky');
                    } else*/ if($lesson['ri_partner'] == 0 && DBRozpis::rozpisSignUp(post('ri_id'), User::getParID()) == true) {
        				notice('Hodina přidána');
        			} else {
        				notice('Už je obsazeno');
        			}
        		} elseif(post('action') == 'signout') {
        			if($lesson['ri_partner'] == 0) {
                        notice('Už je prázdno');
    				} elseif(User::getParID() == $lesson['ri_partner'] || Permissions::check('rozpis', P_OWNED, $data['n_trener'])) {
    					DBRozpis::rozpisSignOut(post('ri_id'));
    					notice('Hodina odebrána');
    				} else {
    					View::viewError(ER_AUTHORIZATION);
    				}
        		}
            }
        }
        
        header_main('Rozpis tréninků');
        if(isset($f))
            notice($f->getMessages());
        
        echo '<style type="text/css">';
        echo '.unit {background:inherit;border:none;
        	vertical-align:top;border-bottom:1px dotted #999;padding:10px 5px;}';
        echo '</style>';
        echo '<table style="width:100%;">';
        $left = true;
        foreach($rozpis as $item) {
        	if(!$item['r_visible'])
        		continue;
        
        	if($left === true) {
        		echo '<tr><td class="unit">';
        	} else {
        		echo '<td class="unit">';
        	}
        	
        	DisplayRozpis::viewRozpisHeader($item);
        	echo '<table style="margin:0 auto;">';
        	echo '<tr><td>Od</td><td>Do</td><td>Tanečník</td></tr>';
        	
        	$r_items = DBRozpis::getRozpisItem($item['r_id']);
        	foreach($r_items as $par) {
        		echo '<form action="', $_SERVER['REQUEST_URI'], '" method="post">';
        		echo '<tr>',
        			'<td>', formatTime($par['ri_od'], 1), '</td>',
        			'<td>', formatTime($par['ri_do'], 1), '</td>',
        			'<td>', echoFullJmeno($par),
        			'<input type="hidden" name="ri_id" value="', $par['ri_id'], '" />',
        			'</td>';
        		if($par['ri_partner'] == 0 && !$item['r_lock'] && !$par['ri_lock'] && Permissions::check('rozpis', P_MEMBER))
        			echo '<td><button type="submit" name="action" value="signup">Rezervovat</button></td>';
        		if($par['ri_partner'] != 0 && !$item['r_lock'] && !$par['ri_lock'] &&
        				((Permissions::check('rozpis', P_MEMBER) && User::getParID() == $par['ri_partner']) ||
        				Permissions::check('rozpis', P_OWNED, $item['r_trener'])))
        				echo '<td><button type="submit" name="action" value="signout">Zrušit</button></td>';
        		echo '</tr>';
        		echo '</form>';
        	}
        	echo '</table>';
        
        	if($left === true) {
        		$left = false;
        		echo '</td>';
        	} else {
        		$left = true;
        		echo '</td></tr>';
        	}
        }
        if($left === false)
        	echo '</tr>';
        echo '</table>';
    }
}
?>