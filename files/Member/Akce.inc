<?php
header_main('Klubové akce');

notice(View::getRedirectMessage());
if(isset($f) && is_object($f))
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
	if(Permissions::check('akce', P_MEMBER) && !$item['a_lock']) {
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