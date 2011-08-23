<?php
User::checkPermissionsError(L_TRENER);

header_main("Správa nabídek");

if(empty($_POST)) {
	include("files/Admin/NabidkaDisplay.inc");
	return;
}

switch(getPostField("action")) {
	case "edit":
		if(is_array(getPostField("nabidka"))) {
			foreach(getPostField("nabidka") as $item) {
				$itemData = DBNabidka::getSingleNabidka($item);
				
				$_POST["id"] = $item;
				$_POST["trener"] = $itemData["n_trener"];
				$_POST["pocet_hod"] = $itemData["n_pocet_hod"];
				$_POST["od"] = $itemData["n_od"];
				$_POST["do"] = $itemData["n_do"];
				$_POST["lock"] = $itemData["n_lock"];
				
				if(User::checkPermissionsBool(L_ADMIN) || $_POST["trener"] == User::getUserName())
					include("files/Admin/NabidkaForm.inc");
				else
					View::viewError(ER_AUTHORIZATION);	
			}
			return;
		}
		break;
	
	case "add":
		include("files/Admin/NabidkaForm.inc");
		return;
	
	case "remove":
		if(is_array(getPostField("nabidka"))) {
			foreach(getPostField("nabidka") as $item) {
				if(User::checkPermissionsBool(L_ADMIN) ||
						(User::getUserID() == DBNabidka::getNabidkaTrenerID($item)))
					DBNabidka::removeNabidka($item);
				else
					View::viewError(ER_AUTHORIZATION);
			}
			notice("Nabídky odebrány");
		}
		break;
	
	case "edit_confirm":
	case "add_confirm":
		$n_id = getPostField("id");
		var_dump($_POST);
		if((getPostField("action") == "edit_confirm" &&
				(User::getUserID() != DBNabidka::getNabidkaTrenerID($n_id) &&
					!User::checkPermissionsBool(L_ADMIN))) ||
				(getPostField("action") == "add_confirm" &&
				(User::getUserID() != getPostField("trener")) &&
					!User::checkPermissionsBool(L_ADMIN)))
			View::viewError(ER_AUTHORIZATION);
		
		if(!getPostField("do"))
			$_POST["do"] = getPostField("od");
		
		$error = false;
		
		ob_start();
		include("files/Admin/NabidkaForm.inc");	
		$html = ob_get_clean();
		
		if(!$error) {
			if(getPostField("action") == "edit_confirm") {
				DBNabidka::editNabidka($n_id, getPostField("trener"), getPostField("pocet_hod"), getPostField("od"),
					getPostField("do"), getPostField("lock"));
				notice("Nabídka úspěšně upravena");
			} else if(getPostField("action") == "add_confirm") {
				DBNabidka::addNabidka(getPostField("trener"), getPostField("pocet_hod"), getPostField("od"),
					getPostField("do"), getPostField("lock"));
				notice("Nabídka úspěšně přidána");
			}
		} else {
			echo $html;
			return;
		}
		break;
		
	case "edit_detail":
		$redirect_str = "";
		$nabidka = getPostArray("nabidka");
		
		if(is_array($nabidka)) {
			if(count($nabidka) > 1) {
				$redirect_str = implode("&id[]=", $nabidka);
				$redirect_str = "id[]=" . $redirect_str;
			} else
				$redirect_str = $nabidka[0];
		}
		
		header("/admin/nabidka-detail?" . $redirect_str);
		return;
		break;
}

include("files/Admin/NabidkaDisplay.inc");
return;
?>