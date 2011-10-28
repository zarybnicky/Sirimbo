<?php
User::checkPermissionsError(L_TRENER);
header_main("Správa nabídek");

$request = explode('/', $_SERVER['REQUEST_URI']);
$action = array_slice($request, 3);
if(empty($action)) $action[] = '';

if($action[0] == 'add') {
	if(empty($_POST)) {
		include('files/Admin/Nabidka/Form.inc');
		return;
	} else {
		if(!User::checkPermissionsBool(L_ADMIN) && User::getUserID() != getPostField("trener"))
			View::viewError(ER_AUTHORIZATION);
		
		$pocet_hod = getPostField("pocet_hod");
		
		$od = getPostField("od_year") . "-" . getPostField("od_month") . "-" . getPostField("od_day");
		$do = getPostField("do_year") . "-" . getPostField("do_month") . "-" . getPostField("do_day");
		if($do == "0000-00-00" || strcmp($od, $do) > 0)
			$do = $od;
		
		$visible = (bool) getPostField("visible");
		if(User::checkPermissionsBool(L_TRENER) && !User::checkPermissionsBool(L_ADMIN))
			$visible = false;
		
		$error = false;
		ob_start();
		include("files/Admin/Nabidka/Form.inc");	
		$html = ob_get_clean();
		
		if(!$error) {
			DBNabidka::addNabidka(getPostField("trener"), $pocet_hod, $od, $do, $visible,
				getPostField("lock"));
			header('Location: /admin/nabidka');
		} else {
			echo $html;
			return;
		}
	}
} elseif($action[0] == 'edit' && is_numeric($action[1])) {
	$id = $action[1];
	
	if($id) {
		$data = DBNabidka::getSingleNabidka($id);
		
		if(!User::checkPermissionsBool(L_ADMIN) && $data["n_trener"] != User::getUserID())
			View::viewError(ER_AUTHORIZATION);
		
		$_POST["id"] = $id;
		$_POST["trener"] = $data["n_trener"];
		$_POST["pocet_hod"] = $data["n_pocet_hod"];
		list($od_year, $od_month, $od_day) = explode("-", $data["n_od"]);
		$_POST["od_year"] = $od_year;
		$_POST["od_month"] = $od_month;
		$_POST["od_day"] = $od_day;
		list($do_year, $do_month, $do_day) = explode("-", $data["n_do"]);
		$_POST["do_year"] = $do_year;
		$_POST["do_month"] = $do_month;
		$_POST["do_day"] = $do_day;
		$_POST["visible"] = $data["n_visible"];
		$_POST["lock"] = $data["n_lock"];
		
		include("files/Admin/Nabidka/Form.inc");
		return;
	}
}

if(empty($_POST)) {
	include("files/Admin/Nabidka/Display.inc");
	return;
}

switch(getPostField("action")) {
	
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
	
	case 'edit':
		$nabidka = getPostField('nabidka');
		if($nabidka[0]) {
			header('Location: /admin/nabidka/edit/' . $nabidka[0]);
			return;
		}
		break;
		
	case "edit_confirm":
		$id = getPostField('id');
		$data = DBNabidka::getSingleNabidka($id);
		
		if(!User::checkPermissionsBool(L_ADMIN) && User::getUserID() != $data['n_trener'])
			View::viewError(ER_AUTHORIZATION);
		
		$items = DBNabidka::getNabidkaItemLessons($id);
		$pocet_hod = getPostField("pocet_hod");
		if($pocet_hod < $items)
			$pocet_hod = $items;
		
		$od = getPostField("od_year") . "-" . getPostField("od_month") . "-" . getPostField("od_day");
		$do = getPostField("do_year") . "-" . getPostField("do_month") . "-" . getPostField("do_day");
		if($do == "0000-00-00" || strcmp($od, $do) > 0)
			$do = $od;
		
		$visible = (bool) getPostField("visible");
		$visible_prev = $data['n_visible'];
		if(User::checkPermissionsBool(L_TRENER) && !User::checkPermissionsBool(L_ADMIN) &&
				$visible != $visible_prev)
			$visible = $visible_prev;
		
		$error = false;
		ob_start();
		include("files/Admin/Nabidka/Form.inc");	
		$html = ob_get_clean();
		
		if(!$error) {
			DBNabidka::editNabidka($id, getPostField("trener"), $pocet_hod, $od, $do, $visible,
				getPostField("lock"));
			notice("Nabídka úspěšně upravena");
		} else {
			header('Location: /admin/nabidka/edit/' . $id);
			return;
		}
		break;
		
	case "edit_detail":
		$nabidka = getPostField("nabidka");
		if($nabidka[0]) {
			header("Location: /admin/nabidka/detail/" . $nabidka[0]);
			return;
		}
		break;
}

include("files/Admin/Nabidka/Display.inc");
return;
?>