<?php
header_main('Nabídka tréninků');
if(is_object($f))
	notice($f->getMessages());

$nabidka = DBNabidka::getNabidka();
if(empty($nabidka)) {
	notice('Žádná nabídka k dispozici');
	return;
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
		if($par['u_id'] === User::getUserID() || $par['u_id'] === User::getPartnerID() ||
				Permissions::check('nabidka', P_OWNED, $item['n_trener'])) {
			echo '<td>';
			if($item['n_lock'] || !Permissions::check('nabidka', P_MEMBER)) {
				echo '<button type="submit" name="un_id" value="',
					$par['p_id'],'-',$item['n_id'],'" disabled="disabled">&times;</button>';
			} else {
				echo '<button type="submit" name="un_id" value="',
					$par['p_id'],'-',$item['n_id'],'">&times;</button>';
			}
			echo '</td>';
		}
		echo '</tr>';
	}
	echo '</table>';
	echo '<div style="text-align:center;">';
	if($item['n_lock'] || !Permissions::check('nabidka', P_MEMBER)) {
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