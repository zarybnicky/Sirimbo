<?php
User::checkPermissionsError(L_TRENER);
header_main("Správa rozpisů");

notice(View::getRedirectMessage());

$request = explode('/', $_SERVER['REQUEST_URI']);
$action = array_slice($request, 3);
if(empty($action)) $action[] = '';

switch($action[0]) {
	case 'add':
		if(empty($_POST)) {
			include('files/Admin/Rozpis/Form.inc');
			return;
		} else {
			if(!User::checkPermissionsBool(L_ADMIN) && User::getUserID() != getPostField("trener"))
				View::viewError(ER_AUTHORIZATION);
			
			$datum = getPostField("year") . "-" . getPostField("month") . "-" . getPostField("day");
		
			$visible = (bool) getPostField("visible");
			if(User::checkPermissionsBool(L_TRENER) && !User::checkPermissionsBool(L_ADMIN)) {
				$visible = false;
				View::setRedirectMessage('Nemáte dostatečná oprávnění ke zviditelnění příspěvku');
			}
			
			$error = false;
			ob_start();
			include("files/Admin/Rozpis/Form.inc");	
			$html = ob_get_clean();
			
			if(!$error) {
				DBRozpis::addRozpis(getPostField("trener"), getPostField("kde"), $datum, $visible,
					getPostField("lock"));
				View::redirectWithMessage('/admin/rozpis', 'Rozpis přidán');
			} else {
				echo $html;
				return;
			}
		}
		break;
	
	case 'edit':
		$id = isset($action[1]) ? $action[1] : '';
		
		if(!$id || !is_numeric($id) || !($data = DBRozpis::getSingleRozpis($id)))
			View::redirectWithMessage('/admin/rozpis', 'Rozpis s takovým ID neexistuje');
		
		if(!User::checkPermissionsBool(L_ADMIN) && $data["r_trener"] != User::getUserID())
			View::viewError(ER_AUTHORIZATION);
		
		if(empty($_POST)) {
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
		} else {
			$datum = getPostField("year") . "-" . getPostField("month") . "-" . getPostField("day");
		
			$visible = (bool) getPostField("visible");
			$visible_prev = $data['r_visible'];
			if(User::checkPermissionsBool(L_TRENER) && !User::checkPermissionsBool(L_ADMIN) &&
					$visible != $visible_prev) {
				$visible = $visible_prev;
				View::setRedirectMessage('Nemáte dostatečná oprávnění ke zviditelnění rozpisu');
			}
					
			$error = false;
			ob_start();
			include("files/Admin/Rozpis/Form.inc");	
			$html = ob_get_clean();
			
			if(!$error) {
				DBRozpis::editRozpis($id, getPostField("trener"), getPostField("kde"), $datum, $visible,
					getPostField("lock"));
				View::redirectWithMessage('/admin/rozpis', 'Rozpis úspěšně upraven');
			} else {
				echo $html;
				return;
			}
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
		
	case "edit_detail":
		$rozpis = getPostField("rozpis");
		if($rozpis[0]) {
			header("Location: /admin/rozpis/detail/" . $rozpis[0]);
			return;
		}
		break;
	
	case "remove":
		if(is_array(getPostField("rozpis"))) {
			foreach(getPostField("rozpis") as $item) {
				if(User::checkPermissionsBool(L_ADMIN) ||
						(User::getUserID() == DBRozpis::getRozpisTrenerID($item))) {
					DBRozpis::removeRozpis($item);
				} else {
					$error = true;
				}
			}
			if(isset($error) && $error)
				View::viewError(ER_AUTHORIZATION);
			
			notice("Rozpisy odebrány");
			include("files/Admin/Rozpis/Display.inc");
			return;
		}
		break;
}

include("files/Admin/Rozpis/Display.inc");
return;
?>