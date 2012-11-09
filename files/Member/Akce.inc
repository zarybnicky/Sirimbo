<?php
User::checkPermissionsError(L_HOST);
header_main('Klubové akce');

if(Request::getID()) {
	DisplayAkce::viewFullAkce(Request::getID());
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
			DBAkce::signUp(User::getUserID(), $id, User::getDatumNarozeni());
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
?>