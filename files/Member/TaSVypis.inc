<?php
User::checkPermissionsError(L_HOST);

if(!is_numeric(getGetField("id")))
	View::viewError(ER_CORRUPT_DATA);

$id = getGetField("id");
$tas = DBTaS::getSingleTaS($id);

if(empty($tas)) {
	echo "Neexistuje žádný takový tábor/soustředění";
	return;
}
$tas_items = DBTas::getTaSItems($id);

echo "<div class=\"t_wrapper\">";
echo "<div class=\"t_info\" style=\"border:1px solid black;\">";
echo "<div class=\"t_jmeno\">Název: ", $tas["t_jmeno"], "</div>";
echo "<div class=\"t_datum\">Datum: ", formatDate($tas["t_od"]);
if($tas["t_od"] != $tas["t_do"])
	echo " - ", formatDate($tas["t_do"]);
echo "</div>";
echo "<div class=\"t_moreinfo\">Další informace: ", $tas["t_info"], "</div>";
echo "<div class=\"t_dokumenty\">Dokumenty:<br/>";
$doku = unserialize($tas["t_dokumenty"]);
if($doku) {
	foreach($doku as $id) {
		$data = DBDokumenty::getSingleDokument($id);
		echo "<a href=\"/member/download?id=", $id, "\">", $data["d_name"], "</a>";
	}
} else {
	echo "Žádné dokumenty";
}
echo "</div>";
echo "</div>";
if(empty($tas_items)) {
	echo "Nikdo není přihlášený";
} else {
	echo "<table>";
	echo "<tr><td>Jméno</td><td>Příjmení</td><td>Rok narození</td></tr>";
	foreach($tas_items as $par) {
		echo "<tr><td>", $par["ti_jmeno"], "</td><td>", $par["ti_prijmeni"], "</td><td>",
			$par["ti_rok_narozeni"], "</td></tr>";
	}
	echo "</table>";
}
echo "</div>";
echo "<form action=\"/member/tas\" method=\"\">";
echo "<input type=\"submit\" value=\"Zpět\" />";
echo "</form>";
?>