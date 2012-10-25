<?php
header_main('Upozornění');
User::checkPermissionsError(L_HOST);

$zpravy = DBNastenka::getNastenka();

if(empty($zpravy)) {
	notice('Žádná upozornění k dispozici');
	return;
}

foreach($zpravy as $data) {
	echo '<div>';
	echo '<div class="up_header">';
	echo '<div style="float:left;"><span class="nadpis">', $data['up_nadpis'], '</span>';
	if(Permissions::canEditNastenka($data['up_kdo'], $data['up_lock']))
		echo ' - <a href="/admin/nastenka/edit/', $data['up_id'], '">Editovat</a>';
	echo '<div style="padding:2px 0;"><div style="float:left;" class="little">skupiny:&nbsp;</div>';
	$skupiny = DBNastenka::getNastenkaSkupiny($data['up_id']);
	foreach($skupiny as $skupina)
		echo '<div class="box" title="', $skupina['ups_popis'], '" ',
			'style="background-color:', Settings::$barvy[$skupina['ups_color']][1], ';"></div>';
	echo '</div></div>';
	echo '<div style="float:right;text-align:right;">';
	echo '<div><span class="little">přidal: </span>';
	echoFullJmeno($data);
	echo '<br/><span class="little">aktualizováno: </span>', formatTimestamp($data['up_aktu']), '</div>';
	echo '</div>';
	echo '<div style="clear:both;"></div>';
	echo '</div>';
	echo '<div style="padding-top:8px;">', $data['up_text'], '</div>';
	echo '</div><hr/>';
}
?>