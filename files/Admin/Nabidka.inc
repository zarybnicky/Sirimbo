<?php
User::checkPermissionsError(L_TRENER);

echo "<div class=\"h_section\">Správa nabídek</div>";

if(empty($_POST)) {
	include("files/Admin/NabidkaDisplay.inc");
	return;
}

switch(getPostField("action")) {
	case "edit":
		if(is_array(getPostField("nabidka"))) {
			foreach(getPostField("nabidka") as $item) {
				$itemData = Database::getSingleNabidka($item);
				
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
						(User::getUserID() == Database::getNabidkaTrenerID($item)))
					Database::removeNabidka($item);
				else
					View::viewError(ER_AUTHORIZATION);
			}
			echo "Nabídky odebrány.<br/>";
		}
		break;
	
	case "edit_confirm":
	case "add_confirm":
		$n_id = getPostField("id");
		var_dump($_POST);
		if((getPostField("action") == "edit_confirm" &&
				(User::getUserID() != Database::getNabidkaTrenerID($n_id) &&
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
				Database::editNabidka($n_id, getPostField("trener"), getPostField("pocet_hod"), getPostField("od"),
					getPostField("do"), getPostField("lock"));
				echo "Nabídka úspěšně upravena.<br/>";
			} else if(getPostField("action") == "add_confirm") {
				Database::addNabidka(getPostField("trener"), getPostField("pocet_hod"), getPostField("od"),
					getPostField("do"), getPostField("lock"));
				echo "Nabídka úspěšně přidána.<br/>";
			}
		} else {
			echo $html;
			return;
		}
		break;

	default:
		include("files/Admin/NabidkaDisplay.inc");
		return;
}

include("files/Admin/NabidkaDisplay.inc");
return;
?>