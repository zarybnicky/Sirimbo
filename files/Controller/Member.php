<?php
class Controller_Member extends Controller_Abstract {
	function __construct() {
		Permissions::checkError('nastenka', P_VIEW);
	}
	function sidebar() {
		$s = new Sidebar();
		
		echo $s->menuHeader();
		echo $s->menuItem('Novinky',			'/member/home');
		echo $s->menuItem('Nástěnka',		'/member/nastenka');
		echo $s->menuItem('Rozpis tréninků',	'/member/rozpis');
		echo $s->menuItem('Nabidka tréninků','/member/nabidka');
		echo $s->menuItem('Klubové akce',	'/member/akce');
		echo $s->menuItem('Dokumenty',		'/member/dokumenty');
		echo $s->menuItem('Žebříček',		'/member/pary');
		echo $s->menuItem('Členové',			'/member/clenove');
		echo $s->menuItem('Profil',			'/member/profil');
	}
    function view($id = null)  {
        DisplayPary::viewPartnerRequests(DBPary::getPartnerRequestsForMe(User::getUserID()),
        	DBPary::getPartnerRequestsByMe(User::getUserID()));
        
        header_main('Novinky');
        notice(View::getRedirectMessage());
        
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