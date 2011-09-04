<?php
User::checkPermissionsError(L_TRENER);

header_main("Správa nabídek");

if(empty($_POST)) {
	include("files/Admin/Nabidka/Display.inc");
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
				list($od_year, $od_month, $od_day) = explode("-", $itemData["n_od"]);
				$_POST["od_year"] = $od_year;
				$_POST["od_month"] = $od_month;
				$_POST["od_day"] = $od_day;
				list($do_year, $do_month, $do_day) = explode("-", $itemData["n_do"]);
				$_POST["do_year"] = $do_year;
				$_POST["do_month"] = $do_month;
				$_POST["do_day"] = $do_day;
				$_POST["lock"] = $itemData["n_lock"];
				
				if(User::checkPermissionsBool(L_ADMIN) || $_POST["trener"] == User::getUserName())
					include("files/Admin/Nabidka/Form.inc");
				else
					View::viewError(ER_AUTHORIZATION);	
			}
			return;
		}
		break;
	
	case "add":
		include("files/Admin/Nabidka/Form.inc");
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
		$od = getPostField("od_year") . "-" . getPostField("od_month") . "-" . getPostField("od_day");
		$do = getPostField("do_year") . "-" . getPostField("do_month") . "-" . getPostField("do_day");
		
		if($do == "0000-00-00" || strcmp($od, $do) > 0)
			$do = $od;
		
		if((getPostField("action") == "edit_confirm" &&
				(User::getUserID() != DBNabidka::getNabidkaTrenerID($n_id) &&
					!User::checkPermissionsBool(L_ADMIN))) ||
				(getPostField("action") == "add_confirm" &&
				(User::getUserID() != getPostField("trener")) &&
					!User::checkPermissionsBool(L_ADMIN)))
			View::viewError(ER_AUTHORIZATION);
		
		$error = false;
		
		ob_start();
		include("files/Admin/Nabidka/Form.inc");	
		$html = ob_get_clean();
		
		if(!$error) {
			if(getPostField("action") == "edit_confirm") {
				DBNabidka::editNabidka($n_id, getPostField("trener"), getPostField("pocet_hod"), $od, $do,
					getPostField("lock"));
				notice("Nabídka úspěšně upravena");
			} else if(getPostField("action") == "add_confirm") {
				DBNabidka::addNabidka(getPostField("trener"), getPostField("pocet_hod"), $od, $do,
					getPostField("lock"));
				notice("Nabídka úspěšně přidána");
			}
		} else {
			echo $html;
			return;
		}
		break;
		
	case "edit_detail":
		$nabidka = getPostField("nabidka");
		if($nabidka[0]) {
			header("Location: /admin/nabidka-detail?id=" . $nabidka[0]);
			return;
		}
		break;
}

include("files/Admin/Nabidka/Display.inc");
return;
?>