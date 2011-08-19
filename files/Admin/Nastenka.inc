<?php
User::checkPermissionsError(L_EDITOR);

echo "<h4>Správa nástěnky</h4>";

if(empty($_POST)) {
	include("files/Admin/NastenkaDisplay.inc");
	return;
}

switch(getPostField("action")) {
	
	case "edit":
		if(is_array(getPostField("nastenka"))) {
			foreach(getPostField("nastenka") as $item) {
				$itemData = Database::getSingleNastenka($item);
				
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
						(User::getUserName() == Database::getNastenkaUserName($item) &&
						!Database::isNastenkaLocked($item)))
					Database::removeNastenka($item);
				else
					View::viewError(ER_AUTHORIZATION);
			}
			echo "Příspěvky odebrány.<br/>";
		}
		break;
	
	case "edit_confirm":
	case "add_confirm":
		$id = getPostField("id");
		if(getPostField("action") == "edit_confirm") {
			if(!User::checkPermissionsBool(L_ADMIN) && (
					User::getUserName() != Database::getNastenkaUserName($id) ||
					Database::isNastenkaLocked($id)))
				break;
		}
		$error = false;
		
		ob_start();
		include("files/Admin/NastenkaForm.inc");	
		$html = ob_get_clean();
		
		if(!$error) {
			if($_POST["action"] == "edit_confirm") {
				Database::editNastenka($id, getPostField("nadpis"),
					getPostField("text"), (getPostField("lock") == "lock") ? 1 : 0);
				echo "Příspěvek úspěšně upraven.<br/>";
			} else if(getPostField("action") == "add_confirm") {
				Database::addNastenka(User::getUserName(), getPostField("nadpis"), getPostField("text"),
					(getPostField("lock") == "lock") ? 1 : 0);
				echo "Příspěvek úspěšně přidán.<br/>";
			}
		} else {
			echo $html;
			return;
		}
		break;
		
	default:
		include("files/Admin/NastenkaDisplay.inc");
		return;
}

include("files/Admin/NastenkaDisplay.inc");
?>