<?php
header_main("Nabídka tréninků");

User::checkPermissionsError(L_USER);

$error = false;
global $isConfirmation;
$isConfirmation = (getPostField("hodiny") && getPostField("id")) || getPostField("un_id");

$nabidka = DBNabidka::getNabidka();

if(empty($nabidka)) {
	notice("Žádná nabídka k dispozici");
	return;
}

if($isConfirmation) {
	$hodiny = getPostField("hodiny");
	$id = getPostField("id");
	$un_id = getPostField("un_id");
	if(!DBNabidka::isNabidkaLocked($id)) {
		if($hodiny) {
			if((DBNabidka::getNabidkaMaxLessons($id) - DBNabidka::getNabidkaItemLessons($id)) >= $hodiny) {
				DBNabidka::addNabidkaItemLessons(User::getUserID(), $id, $hodiny);
				notice("Hodiny přidány");
			} else {
				notice("Tolik volných hodin tu není");
			}
		} elseif($un_id) {
			list($u_id, $n_id) = explode("-", $un_id);
			if(DBNabidka::hasNabidkaLessons($n_id, $u_id) &&
					($u_id == User::getUserID() || User::checkPermissionsBool(L_ADMIN))) {
				DBNabidka::removeNabidkaItem($n_id, $u_id);
				notice("Hodiny odebrány");
			}
		}
	} else {
		notice("Zamčeno :o)");
	}
}

foreach($nabidka as $item) {
	if($item["n_visible"]) {
		$n_items = DBNabidka::getNabidkaItem($item["n_id"]);
		$obsazeno = DBNabidka::getNabidkaItemLessons($item["n_id"]);
		echo "<div class=\"n_wrapper\">";
		echo "<div class=\"n_info\" style=\"border:1px solid black;\">";
		echo "<div class=\"n_trener\">", $item["u_jmeno"], " ", $item["u_prijmeni"], "</div>";
		echo "<div class=\"n_datum\">", $item["n_od"];
		if($item["n_od"] != $item["n_do"])
			echo " - ", $item["n_do"];
		echo "</div>";
		echo "<div class=\"n_hodiny_all\">Celkem hodin: ", $item["n_pocet_hod"], "</div>";
		echo "<div class=\"n_hodiny_obs\">Obsazených hodin: ", $obsazeno, "</div>";
		echo "<div class=\"n_hodiny_vol\">Volných hodin: ", $item["n_pocet_hod"] - $obsazeno, "</div>";
		echo "</div>";
		echo "<table><form action=\"{$_SERVER["REQUEST_URI"]}\" method=\"post\">";
		echo "<tr><td>Tanečník</td><td>Počet hodin</td></tr>";
		foreach($n_items as $par) {
			echo "<tr><td>", $par["u_jmeno"], " ", $par["u_prijmeni"], "</td><td>", $par["ni_pocet_hod"], "</td>";
			if ($par["u_login"] == User::getUserName() || User::checkPermissionsBool(L_ADMIN)) {
				echo "<td><input type=\"hidden\" name=\"un_id\" value=\"", $par["u_id"], "-", $item["n_id"], "\"";
				if($item["n_lock"])
					echo " disabled=\"disabled\"";
				echo " />";
				echo "<input type=\"submit\" value=\"Odstranit\"";
				if($item["n_lock"])
					echo " disabled=\"disabled\"";
				echo " /></td>";
			}
			echo "</tr>";
		}
		echo "</form></table>";
		echo "<div class=\"n_add\"><form action=\"{$_SERVER["REQUEST_URI"]}\" method=\"post\">";
		echo "Počet hodin: <input type=\"text\" name=\"hodiny\" value=\"", getPostField("hodiny"), "\"";
		if($item["n_lock"])
			echo " disabled=\"disabled\"";
		echo " />";
		echo "<input type=\"hidden\" name=\"id\" value=\"", $item["n_id"], "\"";
		if($item["n_lock"])
			echo " disabled=\"disabled\"";
		echo " />";
		echo "<input type=\"submit\" value=\"Přidat\"";
		if($item["n_lock"])
			echo " disabled=\"disabled\"";
		echo " />";
		echo "</form></div>";
		echo "</div>";
	}
}
?>