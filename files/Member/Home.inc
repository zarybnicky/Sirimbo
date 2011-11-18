<?php
User::checkPermissionsError(L_HOST);

DisplayPary::viewPartnerRequests(DBPary::getPartnerRequestsForMe(User::getUserID()),
	DBPary::getPartnerRequestsByMe(User::getUserID()));

header_main('Novinky');

$data = DBNovinky::getLastNovinky(NOVINKY_COUNT);
if(!$data) {
	notice('Žádné novinky');
	return;
}
foreach($data as $item) {
	echo '<div class="no_item">';
	echo '<div class="no_text">', $item['no_text'], '</div>';
	echo '<div class="no_date" style="text-align:right;">', formatTimestamp($item['no_aktu']), '</div>';
	echo '</div><hr/>';
}
?>