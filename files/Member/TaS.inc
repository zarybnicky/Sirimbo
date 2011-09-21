<?php
header_main("Tábory a soustředění");

User::checkPermissionsError(L_USER);

$error = false;
global $isConfirmation;
$isConfirmation = getPostField("action") && getPostField("id");

$tas = DBTaS::getTaS();

if(empty($tas)) {
	Form::notice("Žádné tábory/soustředění");
	return;
}

if($isConfirmation) {
	$action = getPostField("action");
	$id = getPostField("id");
	
	if($action == "signup") {
		if(preg_match("/^19[0-9]{2}|20[0,1,2,3][0-9]$/", getPostField("narozeni"))) {
			DBTaS::signUp(User::getUserID(), $id, getPostField("narozeni"));
		} else {
			notice("Zadejte platný rok narození");
		}
	} elseif($action == "signout") {
		DBTaS::signOut(User::getUserID(), $id);
	} elseif($action == "showsigned") {
		header("Location: /member/tas-vypis?id=" . $id);
		return;
	}
}

foreach($tas as $item) {
	echo "<div class=\"t_wrapper\">";
	echo "<div class=\"t_info\" style=\"border:1px solid black;\">";
	echo "<div class=\"t_jmeno\">Název: ", $item["t_jmeno"], "</div>";
	echo "<div class=\"t_datum\">Datum: ", $item["t_od"];
	if($item["t_od"] != $item["t_do"])
		echo " - ", $item["t_do"];
	echo "</div>";
	echo "<div class=\"t_moreinfo\">Další informace: ", $item["t_info"], "</div>";
	echo "<div class=\"t_dokumenty\">Dokumenty:<br/>";
	$doku = unserialize($item["t_dokumenty"]);
	foreach($doku as $id ) {
		$data = DBDokumenty::getSingleDokument($id);
		echo "<a href=\"/member/download?id=", $id, "\">", $data["d_name"], "</a>";
	}
	echo "</div>";
	echo "</div>";
	echo "<form action=\"{$_SERVER["REQUEST_URI"]}\" method=\"post\">";
	if(DBTas::isUserSignedUp(User::getUserID())) {
		echo "Rok narození: <input type=\"text\" name=\"narozeni\" />";
		echo "<button type=\"submit\" name=\"action\" value=\"signup\">Přihlásit se</button>";
	} else {
		echo "<button type=\"submit\" name=\"action\" value=\"signout\">Odhlásit se</button>";
	}
	echo "<button type=\"submit\" name=\"action\" value=\"showsigned\">Zobrazit přihlášené</button>";
	echo "<input type=\"hidden\" name=\"id\" value=\"", $item["t_id"], "\" />";
	echo "</form>";
	echo "</div>";
}
?>