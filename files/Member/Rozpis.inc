<?php
header_main('Rozpis tréninků');
User::checkPermissionsError(L_HOST);

$error = false;
global $isConfirmation;
$isConfirmation = (post('ri_id') && post('action')) && User::checkPermissionsBool(L_USER);

$rozpis = DBRozpis::getRozpis();

if(empty($rozpis)) {
	notice('Žádná nabídka k dispozici');
	return;
}

if($isConfirmation) {
	$action = post('action');
	$id = post('ri_id');
	
	$lesson = DBRozpis::getRozpisItemLesson($id);
    
	if(!DBRozpis::isRozpisLocked($id)) {
		if($action == 'signup') {
            $partnerka = DBUser::getUserData(User::getPartnerID());
            if(!User::getZaplaceno() || !(strcmp($partnerka['up_plati_do'], date('Y-m-d')) >= 0)) {
                notice('Buď vy nebo váš partner(ka) nemáte zaplacené členské příspěvky');
            } else {
                if($lesson['ri_partner'] == 0 && DBRozpis::rozpisSignUp($id, User::getParID()) == true) {
    				notice('Hodina přidána');
    			} else {
    				notice('Už je obsazeno');
    			}
            }
		} elseif($action == 'signout') {
			if($lesson['ri_partner'] != 0) {
				if((User::checkPermissionsBool(L_ADMIN) || User::getParID() == $lesson['ri_partner'] ||
						(User::checkPermissionsBool(L_TRENER) && $lesson['r_trener'] == User::getUserID())) &&
						DBRozpis::rozpisSignOut($id) == true) {
					notice('Hodina odebrána');
				} else {
					View::viewError(ER_AUTHORIZATION);
				}
			} else {
				notice('Už je prázdno');
			}
		}
	} else {
		notice('Zamčeno :o)');
	}
}
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
		if($par['ri_partner'] == 0 && !$item['r_lock'] && !$par['ri_lock'] && User::checkPermissionsBool(L_USER))
			echo '<td><button type="submit" name="action" value="signup">Rezervovat</button></td>';
		if(($par['ri_partner'] != 0 && (User::checkPermissionsBool(L_ADMIN) ||
				User::getParID() == $par['ri_partner'] ||
				(User::checkPermissionsBool(L_TRENER) && $item['r_trener'] == User::getUserID()))) &&
				!$item['r_lock'] && ! $par['ri_lock'] && User::checkPermissionsBool(L_USER))
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
?>