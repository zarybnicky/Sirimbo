<?php
class Controller_Admin_Akce_Detail implements Controller_Interface {
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
        	$data = DBUser::getUserData($user);
        	
        	if($user != $item["ai_user"] ||
        			($user == "none" ^ $item["ai_user"] == 0)) {
        		list($year) = explode('-', $data['u_narozeni']);
        		DBAkce::editAkceItem($item_id, $user, $year);
        	}
        }
        $items = DBAkce::getAkceItems($id);
        
        if(post("add-user")) {
        	$user = post("add-user");
        	$data = DBUser::getUserData($user);
        	list($year) = explode('-', $data['u_narozeni']);
        	
        	DBAkce::addAkceItem($id, $user, $year);
        	post('add-user', null);
        	$items = DBAkce::getAkceItems($id);
        }
        
        include("files/Admin/AkceDetail/Display.inc");
    }
}
?>