<?php
User::checkPermissionsError(L_TRENER);

header_main("Správa rozpisů");

if(empty($_POST)) {
	include("files/Admin/Rozpis/Display.inc");
	return;
}

switch(getPostField("action")) {
	case "edit":
		$rozpis = getPostField("rozpis");
		if($rozpis[0]) {
			$itemData = DBRozpis::getSingleRozpis($rozpis[0]);
			
			$_POST["id"] = $rozpis[0];
			$_POST["trener"] = $itemData["r_trener"];
			$_POST["kde"] = $itemData["r_kde"];
			list($year, $month, $day) = explode("-", $itemData["r_datum"]);
			$_POST["year"] = $year;
			$_POST["month"] = $month;
			$_POST["day"] = $day;
			$_POST["visible"] = $itemData["r_visible"];
			$_POST["lock"] = $itemData["r_lock"];
			
			if(User::checkPermissionsBool(L_ADMIN) || $_POST["trener"] == User::getUserID())
				include("files/Admin/Rozpis/Form.inc");
			else
				View::viewError(ER_AUTHORIZATION);	
			return;
		}
		break;
	
	case "add":
		include("files/Admin/Rozpis/Form.inc");
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
		$datum = getPostField("year") . "-" . getPostField("month") . "-" . getPostField("day");
		
		if((getPostField("action") == "edit_confirm" &&
				(User::getUserID() != DBRozpis::getRozpisTrenerID($r_id) &&
					!User::checkPermissionsBool(L_ADMIN))) ||
			(getPostField("action") == "add_confirm" &&
				(User::getUserID() != getPostField("trener")) &&
					!User::checkPermissionsBool(L_ADMIN)))
			View::viewError(ER_AUTHORIZATION);
		
		$visible = (bool)getPostField("visible");
		$visible_prev = DBRozpis::isRozpisVisible($r_id);
		if(getPostField("action") == "edit_confirm" && !User::checkPermissionsBool(L_ADMIN) &&
				User::checkPermissionsBool(L_TRENER) && $visible_prev != $visible)
			$visible = $visible_prev;
		
		if(getPostField("action") == "add_confirm" && !User::checkPermissionsBool(L_ADMIN) &&
				User::checkPermissionsBool(L_TRENER))
			$visible = false;
		
		$error = false;
		ob_start();
		include("files/Admin/Rozpis/Form.inc");	
		$html = ob_get_clean();
		
		if(!$error) {
			if(getPostField("action") == "edit_confirm") {
				DBRozpis::editRozpis($r_id, getPostField("trener"), getPostField("kde"), $datum, $visible,
					getPostField("lock"));
				notice("Rozpis úspěšně upraven");
			} else if(getPostField("action") == "add_confirm") {
				DBRozpis::addRozpis(getPostField("trener"), getPostField("kde"), $datum, $visible,
					getPostField("lock"));
				notice("Rozpis úspěšně přidán");
			}
		} else {
			echo $html;
			return;
		}
		break;
		
	case "edit_detail":
		$rozpis = getPostField("rozpis");
		if($rozpis[0]) {
			header("Location: /admin/rozpis-detail?id=" . $rozpis[0]);
			return;
		}
		break;
}

include("files/Admin/Rozpis/Display.inc");
return;
?>