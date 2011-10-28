<?php
header_main('Upozornění');
User::checkPermissionsError(L_HOST);

$zpravy = DBNastenka::getNastenka();

if(empty($zpravy)) {
	notice('Žádná upozornění k dispozici');
	return;
}

foreach($zpravy as $item) {
	echo '<div class="up_item">';
	echo '<div class="up_nadpis" style="float:left;width:50%;"><b>', $item['up_nadpis'], '</b></div>';
	echo '<div class="up_detaily" style="float:right;width:50%;">';
	echo '<div class="up_kdo">';
	echoFullJmeno($item);
	echo '</div>';
	echo '<div class="up_aktu">', formatTimestamp($item['up_aktu']), '</div>';
	echo '<div class="up_barvy">Barvy: ', echoBarvaDisplay($item['up_barvy']), '</div>';
	echo '</div>';
	echo '<div class="up_text" style="clear:right;">', $item['up_text'], '</div>';
	echo '</div><hr/>';
}
?>