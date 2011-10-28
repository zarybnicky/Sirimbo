<?php
header_main("Tábory a soustředění");
User::checkPermissionsError(L_HOST);

$id = array_pop(explode('/', $_SERVER['REQUEST_URI']));
if(is_numeric($id)) {
	DisplayTaS::viewFullTaS($id);
	return;
}

$error = false;
global $isConfirmation;
$isConfirmation = getPostField("action") && getPostField("id");

$tas = DBTaS::getTaS();

if(empty($tas)) {
	notice("Žádné tábory/soustředění");
	return;
}

if($isConfirmation) {
	$action = getPostField("action");
	$id = getPostField("id");
	
	if(User::checkPermissionsBool(L_USER)) {
		if($action == "signup") {
			if(preg_match("/^19[0-9]{2}|20[0,1,2,3][0-9]$/", getPostField("narozeni"))) {
				DBTaS::signUp(User::getUserID(), $id, getPostField("narozeni"));
			} else {
				notice("Zadejte platný rok narození");
			}
		} elseif($action == "signout") {
			DBTaS::signOut(User::getUserID(), $id);
		}
	}
}

foreach($tas as $item) {
	echo "<div class=\"t_wrapper\">";
	
	DisplayTaS::viewTaSHeader($item);
	
	echo "<form action=\"{$_SERVER["REQUEST_URI"]}\" method=\"post\">";
	echo "<input type=\"hidden\" name=\"id\" value=\"", $item["t_id"], "\" />";
	if(User::checkPermissionsBool(L_USER)) {
		if(DBTas::isUserSignedUp(User::getUserID())) {
			echo "Rok narození: <input type=\"text\" name=\"narozeni\" />";
			echo "<button type=\"submit\" name=\"action\" value=\"signup\">Přihlásit se</button>";
		} else {
			echo "<button type=\"submit\" name=\"action\" value=\"signout\">Odhlásit se</button>";
		}
	}
	echo '<a href="/member/tas/', $item['t_id'], '">Zobrazit přihlášené</a>';
	echo "</form>";
	echo "</div>";
}
?>