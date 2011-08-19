<h3><span>Nabídka tréninků</span></h3>
<?php
User::checkPermissionsError(L_USER);

$error = false;
global $isConfirmation;
$isConfirmation = (getPostField("hodiny") && getPostField("id")) || getPostField("un_id");

//TODO: Paging, get(OD,DO)
$nabidka = Database::getNabidka();

if($isConfirmation) {
	$hodiny = getPostField("hodiny");
	$id = getPostField("id");
	$un_id = getPostField("un_id");
	
	if($hodiny) {
		if((Database::getNabidkaMaxLessons($id) - Database::getNabidkaItemLessons($id)) >= $hodiny) {
			Database::addNabidkaItemLessons(User::getUserName(), $id, $hodiny);
			echo "Hodiny přidány";
		} else {
			echo "Tolik volných hodin tu není";
		}
	} elseif($un_id) {
		list($u_id, $n_id) = explode("-", $un_id);
		if(Database::hasNabidkaLessons($n_id, $u_id) &&
				($u_id == User::getUserID() || User::checkPermissionsBool(L_ADMIN))) {
			Database::removeNabidkaItem($n_id, $u_id);
			echo "Hodiny odebrány";
		}
	}
}

foreach($nabidka as $item) {
	$n_items = Database::getNabidkaItem($item["n_id"]);
	$obsazeno = Database::getNabidkaItemLessons($item["n_id"]);
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
//OUT: POST - un_id
			echo "<td><input type=\"hidden\" name=\"un_id\" value=\"", $par["u_id"], "-", $item["n_id"], "\" />";
			echo "<input type=\"submit\" value=\"Odstranit\" /></td>";
		}
		echo "</tr>";
	}
	echo "</form></table>";
	echo "<div class=\"n_add\"><form action=\"{$_SERVER["REQUEST_URI"]}\" method=\"post\">";
	echo "Počet hodin: <input type=\"text\" name=\"hodiny\" value=\"", getPostField("hodiny"), "\" />";
//OUT: POST - id
	echo "<input type=\"hidden\" name=\"id\" value=\"", $item["n_id"], "\" />";
	echo "<input type=\"submit\" value=\"Přidat\" />";
	echo "</form></div>";
	echo "</div>";
}
?>