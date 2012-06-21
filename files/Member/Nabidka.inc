<?php
header_main('Nabídka tréninků');
User::checkPermissionsError(L_HOST);

$error = false;
global $isConfirmation;
$isConfirmation = ((post('hodiny') && post('id')) || post('un_id')) &&
	User::checkPermissionsBool(L_USER);

$nabidka = DBNabidka::getNabidka();

if(empty($nabidka)) {
	notice('Žádná nabídka k dispozici');
	return;
}

if($isConfirmation) {
	$hodiny = post('hodiny');
	$id = post('id');
	$un_id = post('un_id');
	$current_nabidka = DBNabidka::getSingleNabidka($id);
	
	if(!$current_nabidka['n_lock']) {
		if($hodiny) {
			if(($current_nabidka['n_pocet_hod'] - DBNabidka::getNabidkaItemLessons($id)) >= $hodiny) {
				DBNabidka::addNabidkaItemLessons(User::getParID(), $id, $hodiny);
				notice('Hodiny přidány');
				
				unset($_POST['hodiny']);
			} else {
				notice('Tolik volných hodin tu není');
			}
		} elseif($un_id) {
			list($u_id, $n_id) = explode('-', $un_id);
			if(DBNabidka::hasNabidkaLessons($n_id, $u_id) &&
					($u_id == User::getParID() || User::checkPermissionsBool(L_ADMIN))) {
				DBNabidka::removeNabidkaItem($n_id, $u_id);
				notice('Hodiny odebrány');
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
foreach($nabidka as $item) {
	if(!$item['n_visible'])
		continue;
		
	if($left === true) {
		echo '<tr><td class="unit">';
	} else {
		echo '<td class="unit">';
	}
	echo '<form action="', $_SERVER['REQUEST_URI'], '" method="post">';
	
	$obsazeno = DBNabidka::getNabidkaItemLessons($item['n_id']);
	DisplayNabidka::viewNabidkaHeader($item, $obsazeno);
	
	echo '<table style="margin:0 auto;">';
	echo '<tr><td>Tanečník</td><td>Počet hodin</td></tr>';
	
	$n_items = DBNabidka::getNabidkaItem($item['n_id']);
	foreach($n_items as $par) {
		echo '<tr><td>', $par['u_jmeno'], ' ', $par['u_prijmeni'], '</td>',
			'<td>', $par['ni_pocet_hod'], '</td>';
		if($par['u_id'] == User::getUserID() || $par['u_id'] == User::getPartnerID() ||
				User::checkPermissionsBool(L_ADMIN)) {
			echo '<td>';
			if($item['n_lock'] || !User::checkPermissionsBool(L_USER)) {
				echo '<button type="submit" name="un_id" value="',
					$par['p_id'],'-',$item['n_id'],'" disabled="disabled">Odstranit</button>';
			} else {
				echo '<button type="submit" name="un_id" value="',
					$par['p_id'],'-',$item['n_id'],'">Odstranit</button>';
			}
			echo '</td>';
		}
		echo '</tr>';
	}
	echo '</table>';
	echo '<div style="text-align:center;">';
	if($item['n_lock'] || !User::checkPermissionsBool(L_USER)) {
		echo 'Počet hodin: <input type="text" name="hodiny" size="2" value="', post('hodiny'),
			'" disabled="disabled" />';
		echo '<input type="hidden" name="id" value="', $item['n_id'], '" disabled="disabled" />';
		echo '<button type="submit" disabled="disabled">Přidat</button>';
	} else {
		echo 'Počet hodin: <input type="text" name="hodiny" size="2" value="', post('hodiny'), '" />';
		echo '<input type="hidden" name="id" value="', $item['n_id'], '" />';
		echo '<button type="submit">Přidat</button>';
	}
	echo '</div>';
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
