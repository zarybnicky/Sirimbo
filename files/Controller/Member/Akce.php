<?php
class Controller_Member_Akce implements Controller_Interface {
    function view($id = null) {
        if($id) {
        	DisplayAkce::viewFullAkce(Request::getID());
        	return;
        }
        $akce = DBAkce::getAkce();
        if(empty($akce)) {
        	notice('Žádné akce k dispozici');
        	return;
        }
        if(!empty($_POST)) {
        	$data = DBAkce::getSingleAkce(post('id'));
            
            $f = new Form();
            $f->checkBool(!$data['a_lock'], 'Tato akce je zamčená', '');
            $f->checkInArray(post('action'), array('signup', 'signout'), 'Špatná akce', '');
            $f->checkNumeric(post('id'), 'Špatné ID', '');
        	
        	if($f->isValid()) {
        		if(post('action') == 'signup') {
        			DBAkce::signUp(User::getUserID(), post('id'), User::getDatumNarozeni());
        		} elseif(post('action') == 'signout') {
        			DBAkce::signOut(User::getUserID(), post('id'));
        		}
        	}
        }
        
        header_main('Klubové akce');
        if(isset($f))
            notice($f->getMessages());
        
        echo '<style type="text/css">';
        echo '.unit {background:inherit;border:none;
        	vertical-align:top;border-bottom:1px dotted #999;padding:10px 5px;}';
        echo '</style>';
        echo '<table style="width:100%;">';
        $left = true;
        foreach($akce as $item) {
        	if($left) {
        		echo '<tr><td class="unit">';
        	} else {
        		echo '<td class="unit">';
        	}
        	
        	DisplayAkce::viewAkceHeader($item);
        	
        	echo '<form action="', $_SERVER['REQUEST_URI'], '" method="post" style="text-align:center;">';
        	if(User::checkPermissionsBool(L_USER) && !$item['a_lock']) {
        		echo '<input type="hidden" name="id" value="', $item['a_id'], '" />';
        		
        		if(!DBAkce::isUserSignedUp($item['a_id'], User::getUserID())) {
        			echo '<button type="submit" name="action" value="signup">Přihlásit se</button> &bull; ';
        		} else {
        			echo '<button type="submit" name="action" value="signout">Odhlásit se</button> &bull; ';
        		}
        	}
        	echo '<a href="/member/akce/', $item['a_id'], '">Zobrazit přihlášené</a>';
        	echo '</form>';
        	
        	if($left) {
        		$left = false;
        		echo '</td>';
        	} else {
        		$left = true;
        		echo '</td></tr>';
        	}
        }
        if(!$left)
        	echo '</tr>';
        echo '</table>';
    }
}
?>