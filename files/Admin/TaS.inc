<?php
User::checkPermissionsError(L_ADMIN);
header_main("Správa TaS");

$request = explode('/', $_SERVER['REQUEST_URI']);
$action = array_slice($request, 3);
if(empty($action)) $action[] = '';

if($action[0] == 'add') {
	if(empty($_POST)) {
		include("files/Admin/TaS/Form.inc");
		return;
	} else {
		$od = getPostField("od_year") . "-" . getPostField("od_month") . "-" . getPostField("od_day");
		$do = getPostField("do_year") . "-" . getPostField("do_month") . "-" . getPostField("do_day");
		
		if($do == "0000-00-00" || strcmp($od, $do) > 0)
			$do = $od;
		
		$error = false;
		
		ob_start();
		include("files/Admin/TaS/Form.inc");	
		$html = ob_get_clean();
		
		if(!$error) {
			DBTaS::addTaS(getPostField("jmeno"), getPostField("kde"), getPostField("info"), $od, $do,
				getPostField("kapacita"), getPostField("dokumenty"), (getPostField("lock") == "lock") ? 1 : 0);
			header('Location: /admin/tas');
		} else {
			echo $html;
			return;
		}
	}
} elseif($action[0] == 'edit' && is_numeric($action[1])) {
	$id = $action[1];
	
	if($id) {
		$data = DBTaS::getSingleTaS($id);
		
		$_POST["id"] = $id;
		$_POST["jmeno"] = $data["t_jmeno"];
		$_POST["kde"] = $data["t_kde"];
		$_POST["info"] = $data["t_info"];
		list($od_year, $od_month, $od_day) = explode("-", $data["t_od"]);
		$_POST["od_year"] = $od_year;
		$_POST["od_month"] = $od_month;
		$_POST["od_day"] = $od_day;
		list($do_year, $do_month, $do_day) = explode("-", $data["t_do"]);
		$_POST["do_year"] = $do_year;
		$_POST["do_month"] = $do_month;
		$_POST["do_day"] = $do_day;
		$_POST["kapacita"] = $data["t_kapacita"];
		$_POST["dokumenty"] = unserialize($data["t_dokumenty"]);
		$_POST["lock"] = $data["t_lock"];
		
		include("files/Admin/TaS/Form.inc");
		return;
	}
}

if(empty($_POST)) {
	include("files/Admin/TaS/Display.inc");
	return;
}

switch(getPostField("action")) {
	
	case "remove":
		if(is_array(getPostField("tas"))) {
			echo "<form action=\"{$_SERVER["REQUEST_URI"]}\" method=\"POST\">";
			echo "Opravdu chcete odstranit tábor/soustředění:<br/><br/>";
			foreach(getPostField("tas") as $item) {
				echo DBTaS::getTaSName($item) . "<br />";
				echo "<input type=\"hidden\" name=\"tas[]\" value=\"$item\" />";
			}
			echo "<br/>";
			echo "<button type=\"submit\" name=\"action\" value=\"remove_confirm\">",
				"Odstranit</button>";
			echo "</form>";
			echo "<form action=\"{$_SERVER["REQUEST_URI"]}\" method=\"GET\">";
			echo "<button type=\"submit\">Zpět</button>";
			echo "</form>";
			return;
		}
		break;
	
	case "remove_confirm":
		if(is_array(getPostField("tas"))) {
			foreach(getPostField("tas") as $item) {
				if(User::checkPermissionsBool(L_ADMIN))
					DBTaS::removeTaS($item);
				else
					View::viewError(ER_AUTHORIZATION);
			}
			notice("Odebráno");
		}
		break;
	
	case "edit":
		$tas = getPostField('tas');
		if($tas[0]) {
			header('Location: /admin/tas/edit/' . $tas[0]);
			return;
		}
		break;
	
	case "edit_confirm":
		$id = getPostField("id");
		$od = getPostField("od_year") . "-" . getPostField("od_month") . "-" . getPostField("od_day");
		$do = getPostField("do_year") . "-" . getPostField("do_month") . "-" . getPostField("do_day");
		
		if($do == "0000-00-00" || strcmp($od, $do) > 0)
			$do = $od;
		
		$error = false;
		ob_start();
		include("files/Admin/TaS/Form.inc");	
		$html = ob_get_clean();
		
		if(!$error) {
			DBTaS::editTaS($id, getPostField("jmeno"), getPostField("kde"), getPostField("info"), $od, $do,
				getPostField("kapacita"), getPostField("dokumenty"), (getPostField("lock") == "lock") ? 1 : 0);
			notice("Příspěvek úspěšně upraven");
		} else {
			header('Location: /admin/tas/edit/' . $id);
			return;
		}
		break;
		
	case "edit_detail":
		$tas = getPostField("tas");
		if($tas[0]) {
			header("Location: /admin/tas/detail/" . $tas[0]);
			return;
		}
		break;
		
	case "edit_doku":
		$tas = getPostField("tas");
		if($tas[0]) {
			header("Location: /admin/tas/dokumenty/" . $tas[0]);
			return;
		}
		break;
}

include("files/Admin/TaS/Display.inc");
?>