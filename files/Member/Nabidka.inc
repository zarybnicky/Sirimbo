<?php
header_main('Nabídka tréninků');
User::checkPermissionsError(L_HOST);

$error = false;
global $isConfirmation;
$isConfirmation = ((getPostField('hodiny') && getPostField('id')) || getPostField('un_id')) &&
	User::checkPermissionsBool(L_USER);

$nabidka = DBNabidka::getNabidka();

if(empty($nabidka)) {
	notice('Žádná nabídka k dispozici');
	return;
}

if($isConfirmation) {
	$hodiny = getPostField('hodiny');
	$id = getPostField('id');
	$un_id = getPostField('un_id');
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

foreach($nabidka as $item) {
	if($item['n_visible']) {
		$n_items = DBNabidka::getNabidkaItem($item['n_id']);
		$obsazeno = DBNabidka::getNabidkaItemLessons($item['n_id']);
		echo '<div class="n_wrapper">';
		echo '<form action="', $_SERVER['REQUEST_URI'], '" method="post">';
		
		DisplayNabidka::viewNabidkaHeader($item, $obsazeno);
		
		echo '<table>';
		echo '<tr><td>Tanečník</td><td>Počet hodin</td></tr>';
		foreach($n_items as $par) {
			echo '<tr><td>', $par['u_jmeno'], ' ', $par['u_prijmeni'], '</td>',
				'<td>', $par['ni_pocet_hod'], '</td>';
			if($par['u_id'] == User::getUserID() || $par['u_id'] == User::getPartnerID() ||
					User::checkPermissionsBool(L_ADMIN)) {
				echo '<td>';
				if($item['n_lock'] || !User::checkPermissionsBool(L_USER)) {
					echo '<input type="hidden" name="un_id" value="',
						$par['p_id'], '-', $item['n_id'], '" disabled="disabled" />';
					echo '<button type="submit" disabled="disabled">Odstranit</button>';
				} else {
					echo '<input type="hidden" name="un_id" value="',$par['p_id'],'-',$item['n_id'],'"/>';
					echo '<button type="submit">Odstranit</button>';
				}
				echo '</td>';
			}
			echo '</tr>';
		}
		echo '</table>';
		echo '<div class="n_add">';
		if($item['n_lock'] || !User::checkPermissionsBool(L_USER)) {
			echo 'Počet hodin: <input type="text" name="hodiny" value="', getPostField('hodiny'),
				'" disabled="disabled" />';
			echo '<input type="hidden" name="id" value="', $item['n_id'], '" disabled="disabled" />';
			echo '<button type="submit" disabled="disabled">Přidat</button>';
		} else {
			echo 'Počet hodin: <input type="text" name="hodiny" value="', getPostField('hodiny'), '" />';
			echo '<input type="hidden" name="id" value="', $item['n_id'], '" />';
			echo '<button type="submit">Přidat</button>';
		}
		echo '</div>';
		echo '</form></div>';
	}
}
?>