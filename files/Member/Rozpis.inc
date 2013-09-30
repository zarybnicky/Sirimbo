<h1>Rozpis tréninků</h1>
<?php
notice($this->redirect()->getMessage());
if(isset($f) && is_object($f))
	notice($f->getMessages());

$rozpis = DBRozpis::getRozpis();
if(empty($rozpis)) {
	notice('Žádná nabídka k dispozici');
	return;
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
		if($par['ri_partner'] == 0 && !$item['r_lock'] && !$par['ri_lock'] && Permissions::check('rozpis', P_MEMBER))
			echo '<td><button type="submit" name="action" value="signup">Rezervovat</button></td>';
		if($par['ri_partner'] != 0 && !$item['r_lock'] && !$par['ri_lock'] &&
				((Permissions::check('rozpis', P_MEMBER) && User::getParID() == $par['ri_partner']) ||
				Permissions::check('rozpis', P_OWNED, $item['r_trener'])))
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