<?php
User::checkPermissionsError(L_EDITOR);

header_main("Správa nástěnky");

if(empty($_POST)) {
	include("files/Admin/Nastenka/Display.inc");
	return;
}

switch(getPostField("action")) {
	
	case "edit":
		$nastenka = getPostField("nastenka");
		if($nastenka[0]) {
			$itemData = DBNastenka::getSingleNastenka($nastenka[0]);
			
			$_POST["id"] = $nastenka[0];
			$_POST["nadpis"] = $itemData["up_nadpis"];
			$_POST["text"] = $itemData["up_text"];
			$_POST["zluta"] = ($itemData["up_barvy"] & C_ZLUTA);
			$_POST["cervena"] = ($itemData["up_barvy"] & C_CERVENA);
			$_POST["modra"] = ($itemData["up_barvy"] & C_MODRA);
			$_POST["lock"] = $itemData["up_lock"];
			include("files/Admin/Nastenka/Form.inc");
			return;
		}
		break;
		
	case "add":
		include("files/Admin/Nastenka/Form.inc");
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
		
		$zluta = ((bool) getPostField("zluta")) ? C_ZLUTA : 0;
		$cervena = ((bool) getPostField("cervena")) ? C_CERVENA : 0;
		$modra = ((bool) getPostField("modra")) ? C_MODRA : 0;
		$barvy = $zluta | $cervena | $modra;
		
		$lock = (getPostField("lock") == "lock") ? 1 : 0;
		
		$error = false;
		ob_start();
		include("files/Admin/Nastenka/Form.inc");	
		$html = ob_get_clean();
		
		if(!$error) {
			if($_POST["action"] == "edit_confirm") {
				DBNastenka::editNastenka($id, getPostField("nadpis"),
					getPostField("text"), $barvy, (getPostField("lock") == "lock") ? 1 : 0);
				notice("Příspěvek úspěšně upraven");
			} else if(getPostField("action") == "add_confirm") {
				DBNastenka::addNastenka(User::getUserID(), getPostField("nadpis"),
					getPostField("text"), $barvy, $lock);
				notice("Příspěvek úspěšně přidán");
			}
		} else {
			echo $html;
			return;
		}
		break;
}

include("files/Admin/Nastenka/Display.inc");
?>