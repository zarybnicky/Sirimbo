<?php
header_main('Upozornění');
User::checkPermissionsError(L_HOST);

$zpravy = DBNastenka::getNastenka();

if(empty($zpravy)) {
	notice('Žádná upozornění k dispozici');
	return;
}

foreach($zpravy as $data) {
	echo '<div class="up_item">';
	echo '<div class="up_nadpis" style="float:left;width:50%;"><b>', $data['up_nadpis'], '</b>';
	if(Permissions::canEditNastenka($data['up_kdo'], $data['up_lock']))
		echo ' - <a href="/admin/nastenka/edit/', $data['up_id'], '">Editovat</a>';;
	echo '</div>';
	echo '<div class="up_detaily" style="float:right;width:50%;">';
	echo '<div class="up_kdo">';
	echoFullJmeno($data);
	echo '</div>';
	echo '<div class="up_aktu">', formatTimestamp($data['up_aktu']), '</div>';
	echo '<div class="up_barvy">Barvy: ', echoBarvaDisplay($data['up_barvy']), '</div>';
	echo '</div>';
	echo '<div class="up_text" style="clear:right;">', $data['up_text'], '</div>';
	echo '</div><hr/>';
}
?>