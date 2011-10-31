<?php
User::checkPermissionsError(L_HOST);
header_main('Klubové akce');

$id = array_pop(explode('/', $_SERVER['REQUEST_URI']));
if(is_numeric($id)) {
	DisplayAkce::viewFullAkce($id);
	return;
}

$error = false;
global $isConfirmation;
$isConfirmation = getPostField('action') && getPostField('id');

$akce = DBAkce::getAkce();

if(empty($akce)) {
	notice('Žádné akce k dispozici');
	return;
}

if($isConfirmation) {
	$action = getPostField('action');
	$id = getPostField('id');
	
	if(User::checkPermissionsBool(L_USER)) {
		if($action == 'signup') {
			if(preg_match('/^19[0-9]{2}|20[0,1,2,3][0-9]$/', getPostField('narozeni'))) {
				DBAkce::signUp(User::getUserID(), $id, getPostField('narozeni'));
			} else {
				notice('Zadejte platný rok narození');
			}
		} elseif($action == 'signout') {
			DBAkce::signOut(User::getUserID(), $id);
		}
	}
}

foreach($akce as $item) {
	echo '<div class="a_wrapper">';
	
	DisplayAkce::viewAkceHeader($item);
	
	echo '<form action="', $_SERVER['REQUEST_URI'], '" method="post">';
	echo '<input type="hidden" name="id" value="', $item['a_id'], '" />';
	if(User::checkPermissionsBool(L_USER)) {
		if(DBTas::isUserSignedUp(User::getUserID())) {
			echo 'Rok narození: <input type="text" name="narozeni" />';
			echo '<button type="submit" name="action" value="signup">Přihlásit se</button>';
		} else {
			echo '<button type="submit" name="action" value="signout">Odhlásit se</button>';
		}
	}
	echo '<a href="/member/akce/"', $item['a_id'], '">Zobrazit přihlášené</a>';
	echo '</form>';
	echo '</div>';
}
?>