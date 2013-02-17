<?php
class Controller_Member implements Controller_Interface {
	function __construct() {
		Permissions::checkError('nastenka', P_VIEW);
	}
    function view($id = null)  {
        notice(View::getRedirectMessage());
        
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
        	echo '<div class="no_date" style="text-align:right;">';
        	if(Permissions::check('nastenka', P_OWNED))
        		echo '<a href="/admin/novinky/remove?id=', $item['no_id'], '">Odstranit</a> - ';
        	echo formatTimestamp($item['no_aktu']), '</div>';
        	echo '</div><hr/>';
        }
    }
}
?>