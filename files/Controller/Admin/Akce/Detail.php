<?php
include_once('files/Controller/Admin/Akce.php');
class Controller_Admin_Akce_Detail extends Controller_Admin_Akce {
	function __construct() {
		Permissions::checkError('akce', P_OWNED);
	}
    function view($id = null) {
        if(!$id || !($akce = DBAkce::getSingleAkce($id)))
        	View::redirect('/admin/akce', 'Akce s takovým ID neexistuje');
            
        header_main("Správa akcí");
        notice(View::getRedirectMessage());
        
        $items = DBAkce::getAkceItems($id);
        $users = DBUser::getActiveUsers();
        
        if(empty($_POST)) {
        	include("files/Admin/AkceDetail/Display.inc");
        	return;
        }
        
        if(post("remove") > 0) {
        	DBAkce::removeAkceItem(post("remove"));
        	$items = DBAkce::getAkceItems($id);
        }
        
        foreach($items as $item) {
        	$item_id = $item["ai_id"];
        	$user = post($item_id . '-user');
        	
        	if($user != $item["ai_user"]) {
        		$data = DBUser::getUserData($user);
        		list($year) = explode('-', $data['u_narozeni']);
        		DBAkce::editAkceItem($item_id, $user, $year);
        	}
        }
        $items = DBAkce::getAkceItems($id);
        
        if(is_numeric(post("add-user"))) {
        	$user = post("add-user");
        	$data = DBUser::getUserData($user);
        	list($year) = explode('-', $data['u_narozeni']);
        	
        	DBAkce::addAkceItem($id, $user, $year);
        	post('add-user', -1	);
        	$items = DBAkce::getAkceItems($id);
        }
        
        include("files/Admin/AkceDetail/Display.inc");
    }
}
?>