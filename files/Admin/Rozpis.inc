<?php
User::checkPermissionsError(L_TRENER);

header_main("Správa rozpisů");

if(empty($_POST)) {
	include("files/Admin/RozpisDisplay.inc");
	return;
}

switch(getPostField("action")) {
	case "edit":
		if(is_array(getPostField("rozpis"))) {
			foreach(getPostField("rozpis") as $item) {
				$itemData = DBRozpis::getSingleRozpis($item);
				
				$_POST["id"] = $item;
				$_POST["trener"] = $itemData["r_trener"];
				$_POST["kde"] = $itemData["r_kde"];
				$_POST["datum"] = $itemData["r_datum"];
				$_POST["od"] = $itemData["r_od"];
				$_POST["do"] = $itemData["r_do"];
				$_POST["lock"] = $itemData["r_lock"];
				
				if(User::checkPermissionsBool(L_ADMIN) || $_POST["trener"] == User::getUserName())
					include("files/Admin/RozpisForm.inc");
				else
					View::viewError(ER_AUTHORIZATION);	
			}
			return;
		}
		break;
	
	case "add":
		include("files/Admin/RozpisForm.inc");
		return;
	
	case "remove":
		if(is_array(getPostField("rozpis"))) {
			foreach(getPostField("rozpis") as $item) {
				if(User::checkPermissionsBool(L_ADMIN) ||
						(User::getUserID() == DBRozpis::getRozpisTrenerID($item)))
					DBRozpis::removeRozpis($item);
				else
					View::viewError(ER_AUTHORIZATION);
			}
			notice("Rozpisy odebrány");
		}
		break;
	
	case "edit_confirm":
	case "add_confirm":
		$r_id = getPostField("id");
		
		if((getPostField("action") == "edit_confirm" &&
				(User::getUserID() != DBRozpis::getRozpisTrenerID($r_id) &&
					!User::checkPermissionsBool(L_ADMIN))) ||
			(getPostField("action") == "add_confirm" &&
				(User::getUserID() != getPostField("trener")) &&
					!User::checkPermissionsBool(L_ADMIN)))
			View::viewError(ER_AUTHORIZATION);
		
		$error = false;
		
		ob_start();
		include("files/Admin/RozpisForm.inc");	
		$html = ob_get_clean();
		
		if(!$error) {
			if(getPostField("action") == "edit_confirm") {
				DBRozpis::editRozpis($r_id, getPostField("trener"), getPostField("kde"), getPostField("datum"),
					getPostField("od"), getPostField("do"), getPostField("lock"));
				notice("Rozpis úspěšně upraven");
			} else if(getPostField("action") == "add_confirm") {
				DBRozpis::addRozpis(getPostField("trener"), getPostField("kde"), getPostField("datum"),
					getPostField("od"), getPostField("do"), getPostField("lock"));
				notice("Rozpis úspěšně přidán");
			}
		} else {
			echo $html;
			return;
		}
		break;
}

include("files/Admin/RozpisDisplay.inc");
return;
?>