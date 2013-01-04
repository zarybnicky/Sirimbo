<?php
header_main('Upozornění');
User::checkPermissionsError(L_HOST);

$pager = new Paging(new PagingAdapterDBSelect('DBNastenka'));
$pager->setCurrentPageField('p');
$pager->setItemsPerPageField('c');
$pager->setDefaultItemsPerPage(10);
$pager->setPageRange(5);
$items = $pager->getItems();

if(empty($items)) {
	notice('Žádná upozornění k dispozici');
	return;
}

foreach($items as $data) {
	echo '<div>';
	echo '<div class="up_header">';
	echo '<div style="float:left;"><span class="nadpis">', $data['up_nadpis'], '</span>';
	if(Permissions::canEditNastenka($data['up_kdo'], $data['up_lock']))
		echo ' - <a href="/admin/nastenka/edit/', $data['up_id'], '">Editovat</a>';
	echo '<div style="padding:2px 0;"><div style="float:left;" class="little">skupiny:&nbsp;</div>';
	$skupiny = DBNastenka::getNastenkaSkupiny($data['up_id']);
	foreach($skupiny as $skupina)
		echo getColorBox($skupina['ups_color'], $skupina['ups_popis']);
	echo '</div></div>';
	echo '<div style="float:right;text-align:right;">';
	echo '<div><span class="little">přidal: </span>';
	echoFullJmeno($data);
	echo '<br/><span class="little">aktualizováno: </span>', formatTimestamp($data['up_aktu']), '</div>';
	echo '</div>';
	echo '<div style="clear:both;"></div>';
	echo '</div>';
	echo '<div style="padding-top:8px;">', stripslashes($data['up_text']), '</div>';
	echo '</div><hr/>';
}
echo '<div style="text-align:center;">', $pager->getNavigation(), '</div>';
?>