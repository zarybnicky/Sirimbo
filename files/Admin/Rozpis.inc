<?php
User::checkPermissionsError(L_TRENER);

echo "<div class=\"h_section\">Správa rozpisů</div>";

if(empty($_POST)) {
	include("files/Admin/RozpisDisplay.inc");
	return;
}

switch(getPostField("action")) {
	case "edit":
		if(is_array(getPostField("rozpis"))) {
			foreach(getPostField("rozpis") as $item) {
				$itemData = Database::getSingleRozpis($item);
				
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
						(User::getUserID() == Database::getRozpisTrenerID($item)))
					Database::removeRozpis($item);
				else
					View::viewError(ER_AUTHORIZATION);
			}
			echo "Rozpisy odebrány.<br/>";
		}
		break;
	
	case "edit_confirm":
	case "add_confirm":
		$r_id = getPostField("id");
		
		if((getPostField("action") == "edit_confirm" &&
				(User::getUserID() != Database::getRozpisTrenerID($r_id) &&
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
				Database::editRozpis($r_id, getPostField("trener"), getPostField("kde"), getPostField("datum"),
					getPostField("od"), getPostField("do"), getPostField("lock"));
				echo "Rozpis úspěšně upraven.<br/>";
			} else if(getPostField("action") == "add_confirm") {
				Database::addRozpis(getPostField("trener"), getPostField("kde"), getPostField("datum"),
					getPostField("od"), getPostField("do"), getPostField("lock"));
				echo "Rozpis úspěšně přidán.<br/>";
			}
		} else {
			echo $html;
			return;
		}
		break;

	default:
		include("files/Admin/RozpisDisplay.inc");
		return;
}

include("files/Admin/RozpisDisplay.inc");
return;
?>