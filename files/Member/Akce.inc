<?php
User::checkPermissionsError(L_HOST);
header_main('Klubové akce');

$request = explode('/', $_SERVER['REQUEST_URI']);
$id = array_pop($request);
if(is_numeric($id)) {
	DisplayAkce::viewFullAkce($id);
	return;
}

$akce = DBAkce::getAkce();

if(empty($akce)) {
	notice('Žádné akce k dispozici');
	return;
}

$error = false;
global $isConfirmation;
$isConfirmation = post('action') && post('id');

if($isConfirmation) {
	$action = post('action');
	$id = post('id');
	$data = DBAkce::getSingleAkce($id);
	
	if(User::checkPermissionsBool(L_USER) && !$data['a_lock']) {
		if($action == 'signup') {
			if(preg_match('/^19[0-9]{2}|20[0,1,2,3][0-9]$/', post('narozeni'))) {
				DBAkce::signUp(User::getUserID(), $id, post('narozeni'));
			} else {
				notice('Zadejte platný rok narození');
			}
		} elseif($action == 'signout') {
			DBAkce::signOut(User::getUserID(), $id);
		}
	}
}

echo '<style type="text/css">';
echo '.unit {background:inherit;border:none;
	vertical-align:top;border-bottom:1px dotted #999;padding:10px 5px;}';
echo '</style>';
echo '<table style="width:100%;">';
$left = true;
foreach($akce as $item) {
	if($left === true) {
		echo '<tr><td class="unit">';
	} else {
		echo '<td class="unit">';
	}
	
	DisplayAkce::viewAkceHeader($item);
	
	echo '<form action="', $_SERVER['REQUEST_URI'], '" method="post" style="text-align:center;">';
	if(User::checkPermissionsBool(L_USER) && !$item['a_lock']) {
		echo '<input type="hidden" name="id" value="', $item['a_id'], '" />';
		
		if(!DBAkce::isUserSignedUp($item['a_id'], User::getUserID())) {
			echo 'Rok narození: <input type="text" size="4" name="narozeni" />';
			echo '<button type="submit" name="action" value="signup">Přihlásit se</button>';
		} else {
			echo '<button type="submit" name="action" value="signout">Odhlásit se</button>';
		}
		echo '<br/>';
	}
	echo '<a href="/member/akce/', $item['a_id'], '">Zobrazit přihlášené</a>';
	echo '</form>';
	
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