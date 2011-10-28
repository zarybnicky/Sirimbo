<?php
User::checkPermissionsError(L_TRENER);
header_main("Správa rozpisů");

$request = explode('/', $_SERVER['REQUEST_URI']);
$action = array_slice($request, 3);
if(empty($action)) $action[] = '';

if($action[0] == 'add') {
	if(empty($_POST)) {
		include('files/Admin/Rozpis/Form.inc');
		return;
	} else {
		$datum = getPostField("year") . "-" . getPostField("month") . "-" . getPostField("day");
		
		if(!User::checkPermissionsBool(L_ADMIN) && User::getUserID() != getPostField("trener"))
			View::viewError(ER_AUTHORIZATION);
		
		$visible = (bool) getPostField("visible");
		if(User::checkPermissionsBool(L_TRENER) && !User::checkPermissionsBool(L_ADMIN))
			$visible = false;
		
		$error = false;
		ob_start();
		include("files/Admin/Rozpis/Form.inc");	
		$html = ob_get_clean();
		
		if(!$error) {
			DBRozpis::addRozpis(getPostField("trener"), getPostField("kde"), $datum, $visible,
				getPostField("lock"));
			header('Location: /admin/rozpis');
		} else {
			echo $html;
			return;
		}
	}
} elseif($action[0] == 'edit' && is_numeric($action[1])) {
	$id = $action[1];
	
	if($id) {
		$data = DBRozpis::getSingleRozpis($id);
		
		if(!User::checkPermissionsBool(L_ADMIN) && $data['r_trener'] != User::getUserID())
			View::viewError(ER_AUTHORIZATION);
		
		$_POST["id"] = $id;
		$_POST["trener"] = $data["r_trener"];
		$_POST["kde"] = $data["r_kde"];
		list($year, $month, $day) = explode("-", $data["r_datum"]);
		$_POST["year"] = $year;
		$_POST["month"] = $month;
		$_POST["day"] = $day;
		$_POST["visible"] = $data["r_visible"];
		$_POST["lock"] = $data["r_lock"];
		
		include("files/Admin/Rozpis/Form.inc");
		return;
	}
}

if(empty($_POST)) {
	include("files/Admin/Rozpis/Display.inc");
	return;
}

switch(getPostField("action")) {
	case 'edit':
		$rozpis = getPostField('rozpis');
		if($rozpis[0]) {
			header('Location: /admin/rozpis/edit/' . $rozpis[0]);
			return;
		}
		break;
	
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
		$id = getPostField("id");
		$data = DBRozpis::getSingleRozpis($id);

		$datum = getPostField("year") . "-" . getPostField("month") . "-" . getPostField("day");
		
		if((!User::checkPermissionsBool(L_ADMIN) && User::getUserID() != $data['r_trener']))
			View::viewError(ER_AUTHORIZATION);
		
		$visible = (bool) getPostField("visible");
		$visible_prev = $data['r_visible'];
		
		if(User::checkPermissionsBool(L_TRENER) && !User::checkPermissionsBool(L_ADMIN) &&
				$visible != $visible_prev)
			$visible = $visible_prev;
					
		$error = false;
		ob_start();
		include("files/Admin/Rozpis/Form.inc");	
		$html = ob_get_clean();
		
		if(!$error) {
			DBRozpis::editRozpis($id, getPostField("trener"), getPostField("kde"), $datum, $visible,
				getPostField("lock"));
			notice("Rozpis úspěšně upraven");
		} else {
			header('Location: /admin/rozpis/edit/' . $id);
			return;
		}
		break;
		
	case "edit_detail":
		$rozpis = getPostField("rozpis");
		if($rozpis[0]) {
			header("Location: /admin/rozpis/detail/" . $rozpis[0]);
			return;
		}
		break;
}

include("files/Admin/Rozpis/Display.inc");
return;
?>