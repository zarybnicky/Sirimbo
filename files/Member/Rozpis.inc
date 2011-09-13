<?php
header_main("Rozpis tréninků");

User::checkPermissionsError(L_USER);

$error = false;
global $isConfirmation;
$isConfirmation = getPostField("ri_id") && getPostField("action");

$rozpis = DBRozpis::getRozpis();

if(empty($rozpis)) {
	notice("Žádná nabídka k dispozici");
	return;
}

echo "<br/>";
if($isConfirmation) {
	$action = getPostField("action");
	$id = getPostField("ri_id");
	
	$lesson = DBRozpis::getRozpisItemLesson($id);
	$lesson = $lesson[0];
	if(!DBRozpis::isRozpisLocked($id)) {
		if($action == "signup") {
			if($lesson["ri_partner"] == 0 && DBRozpis::rozpisSignUp($id, User::getUserID()) == true) {
				notice("Hodina přidána");
			} else {
				notice("Už je obsazeno");
			}
		} elseif($action == "signout") {
			if($lesson["ri_partner"] != 0) {
				if((User::checkPermissionsBool(L_ADMIN) ||
						User::getUserID() == $lesson["ri_partner"] ||
						(User::checkPermissionsBool(L_TRENER) && $lesson["r_trener"] == User::getUserID())) &&
						DBRozpis::rozpisSignOut($id, User::getUserID()) == true) {
					notice("Hodina odebrána");
				} else {
					View::viewError(ER_AUTHORIZATION);
				}
			} else {
				notice("Už je prázdno");
			}
		}
	} else {
		notice("Zamčeno :o)");
	}
}

foreach($rozpis as $item) {
	if($item["r_visible"]) {
		$r_items = DBRozpis::getRozpisItem($item["r_id"]);
		echo "<div class=\"r_wrapper\">";
		echo "<div class=\"r_info\" style=\"float:left;border:1px solid black;\">";
		echo "<div class=\"r_datum\">", $item["r_datum"], "</div>";
		echo "<div class=\"r_kde\">", $item["r_kde"], "</div>";
		echo "<div class=\"r_trener\">", $item["u_jmeno"], " ", $item["u_prijmeni"], "</div>";
		echo "</div>";
		echo "<table>";
		echo "<tr><td>Od</td><td>Do</td><td>Tanečník</td><td></td><td></td></tr>";
		foreach($r_items as $par) {
			echo "<form action=\"{$_SERVER["REQUEST_URI"]}\" method=\"post\">";
			echo "<tr><td>", $par["ri_od"], "</td><td>", $par["ri_do"], "</td><td>",
				$par["u_jmeno"], " ", $par["u_prijmeni"], "</td>";
			echo "<td>";
			echo "<input type=\"hidden\" name=\"ri_id\" value=\"", $par["ri_id"], "\" />";
			if($par["ri_partner"] == 0 && !$item["r_lock"] && !$item["ri_lock"])
				echo "<button type=\"submit\" name=\"action\" value=\"signup\">Rezervovat</button>";
			else
				echo "<button type=\"submit\" name=\"action\" value=\"signup\" disabled=\"disabled\">Rezervovat</button>";
			echo "</td>";
			echo "<td>";
			if(($par["ri_partner"] != 0 && (User::checkPermissionsBool(L_ADMIN) ||
					User::getUserID() == $par["ri_partner"] ||
					(User::checkPermissionsBool(L_TRENER) && $item["r_trener"] == User::getUserID()))) &&
					!$item["r_lock"] && ! $par["ri_lock"])
					echo "<button type=\"submit\" name=\"action\" value=\"signout\">Zrušit</button>";
			else
				echo "<button type=\"submit\" name=\"action\" value=\"signout\" disabled=\"disabled\">Zrušit</button>";
			echo "</td>";
			echo "</tr>";
			echo "</form>";
		}
		echo "</table>";
		echo "</div>";
	}
}
?>