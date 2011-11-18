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
			if(!Permissions::canEditRozpis(getPostField("trener")))
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
				
				if($visible) {
					$trener_data = DBUser::getUserData(getPostField("trener"));
					$trener_name = $trener_data['u_jmeno'] . ' ' . $trener_data['u_prijmeni'];
					
					DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' přidal rozpis ' .
						formatDate($datum) . ' s trenérem ' . $trener_name);
				}
				View::redirect('/admin/rozpis', 'Rozpis přidán');
			} else {
				echo $html;
				return;
			}
		}
		break;
	
	case 'edit':
		$id = isset($action[1]) ? $action[1] : '';
		
		if(!$id || !is_numeric($id) || !($data = DBRozpis::getSingleRozpis($id)))
			View::redirect('/admin/rozpis', 'Rozpis s takovým ID neexistuje');
		
		if(!Permissions::canEditRozpis($data["r_trener"]))
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
					
				$trener_data = DBUser::getUserData(getPostField("trener"));
				$trener_name = $trener_data['u_jmeno'] . ' ' . $trener_data['u_prijmeni'];
				if($visible) {
					if(!$visible_prev)
						DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' přidal rozpis ' .
							formatDate($datum) . ' s trenérem ' . $trener_name);
					else
						DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' upravil rozpis ' .
							formatDate($datum) . ' s trenérem ' . $trener_name);
				} elseif(!$visible && $visible_prev && strcmp($datum, date('Y-m-d')) > 0) {
						DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' zrušil rozpis ' .
							formatDate($datum) . ' s trenérem ' . $trener_name);
				}
				View::redirect('/admin/rozpis', 'Rozpis úspěšně upraven');
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
				$trener = DBRozpis::getRozpisTrener($item);
				$data = DBRozpis::getSingleRozpis($item);
				
				if(Permissions::canEditRozpis($trener['u_id'])) {
					DBRozpis::removeRozpis($item);
					if(strcmp($data['r_datum'], date('Y-m-d')) > 0 && $data['n_visible'])
						DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' zrušil rozpis ' .
							formatDate($data['r_datum']) . ' s trenérem ' .
							$trener['u_jmeno'] . ' ' . $trener['u_prijmeni']);
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