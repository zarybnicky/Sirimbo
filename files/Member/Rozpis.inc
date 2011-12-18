<?php
header_main('Rozpis tréninků');
User::checkPermissionsError(L_HOST);

$error = false;
global $isConfirmation;
$isConfirmation = (getPostField('ri_id') && getPostField('action')) && User::checkPermissionsBool(L_USER);

$rozpis = DBRozpis::getRozpis();

if(empty($rozpis)) {
	notice('Žádná nabídka k dispozici');
	return;
}

if($isConfirmation) {
	$action = getPostField('action');
	$id = getPostField('ri_id');
	
	$lesson = DBRozpis::getRozpisItemLesson($id);
	$lesson = $lesson[0];
	if(!DBRozpis::isRozpisLocked($id)) {
		if($action == 'signup') {
			if($lesson['ri_partner'] == 0 && DBRozpis::rozpisSignUp($id, User::getParID()) == true) {
				notice('Hodina přidána');
			} else {
				notice('Už je obsazeno');
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

foreach($rozpis as $item) {
	if($item['r_visible']) {
		echo '<div style="float:left;padding:0 10px;">';
		DisplayRozpis::viewRozpisHeader($item);
		echo '<table style="margin: 0 auto;">';
		echo '<tr><td>Od</td><td>Do</td><td>Tanečník</td></tr>';
		
		$r_items = DBRozpis::getRozpisItem($item['r_id']);
		foreach($r_items as $par) {
			echo '<form action="', $_SERVER['REQUEST_URI'], '" method="post">';
			echo '<tr>';
			echo '<td>', formatTime($par['ri_od'], 1), '</td><td>', formatTime($par['ri_do'], 1), '</td>',
				'<td>', $par['u_jmeno'], ' ', $par['u_prijmeni'],
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
		echo '</table><hr/></div>';
	}
}
?>