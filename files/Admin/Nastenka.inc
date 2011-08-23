<?php
User::checkPermissionsError(L_EDITOR);

header_main("Správa nástěnky");

if(empty($_POST)) {
	include("files/Admin/NastenkaDisplay.inc");
	return;
}

switch(getPostField("action")) {
	
	case "edit":
		if(is_array(getPostField("nastenka"))) {
			foreach(getPostField("nastenka") as $item) {
				$itemData = DBNastenka::getSingleNastenka($item);
				
				$_POST["id"] = $item;
				$_POST["nadpis"] = $itemData["up_nadpis"];
				$_POST["text"] = $itemData["up_text"];
				$_POST["lock"] = $itemData["up_lock"];
				include("files/Admin/NastenkaForm.inc");
			}
			return;
		}
		break;
		
	case "add":
		include("files/Admin/NastenkaForm.inc");
		return;
	
	case "remove":
		if(is_array(getPostField("nastenka"))) {
			foreach(getPostField("nastenka") as $item) {
				if(User::checkPermissionsBool(L_ADMIN) ||
						(User::getUserName() == DBNastenka::getNastenkaUserName($item) &&
						!DBNastenka::isNastenkaLocked($item)))
					DBNastenka::removeNastenka($item);
				else
					View::viewError(ER_AUTHORIZATION);
			}
			notice("Příspěvky odebrány");
		}
		break;
	
	case "edit_confirm":
	case "add_confirm":
		$id = getPostField("id");
		if(getPostField("action") == "edit_confirm") {
			if(!User::checkPermissionsBool(L_ADMIN) && (
					User::getUserName() != DBNastenka::getNastenkaUserName($id) ||
					DBNastenka::isNastenkaLocked($id)))
				break;
		}
		$error = false;
		
		ob_start();
		include("files/Admin/NastenkaForm.inc");	
		$html = ob_get_clean();
		
		if(!$error) {
			if($_POST["action"] == "edit_confirm") {
				DBNastenka::editNastenka($id, getPostField("nadpis"),
					getPostField("text"), (getPostField("lock") == "lock") ? 1 : 0);
				notice("Příspěvek úspěšně upraven");
			} else if(getPostField("action") == "add_confirm") {
				DBNastenka::addNastenka(User::getUserName(), getPostField("nadpis"), getPostField("text"),
					(getPostField("lock") == "lock") ? 1 : 0);
				notice("Příspěvek úspěšně přidán");
			}
		} else {
			echo $html;
			return;
		}
		break;
}

include("files/Admin/NastenkaDisplay.inc");
?>