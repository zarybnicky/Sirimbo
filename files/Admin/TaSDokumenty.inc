<?php
User::checkPermissionsError(L_ADMIN);

if(!is_numeric(getGetField("id")) && getGetField("id") > 0 && getGetField("id") < 2147483647)
	View::viewError(ER_CORRUPT_DATA);
if(!($tas = DBTaS::getSingleTaS(getGetField("id"))))
	View::viewError(ER_CORRUPT_DATA);
$doku = unserialize($tas["t_dokumenty"]);

header_main("Správa TaS");

if(empty($_POST)) {
	include("files/Admin/TaSDokumenty/Display.inc");
	return;
}

if(getPostField("remove") > 0) {
	$to_remove = array_search(getPostField("remove"), $doku);
	
	if(is_int($to_remove)) {
		unset($doku[$to_remove]);
		$doku = array_values($doku);
	}
	
	DBTaS::editTaS($tas["t_id"], $tas["t_jmeno"], $tas["t_kde"], $tas["t_info"], $tas["t_od"], $tas["t_do"],
		$tas["t_kapacita"], serialize($doku), $tas["t_kapacita"]);
	
	$tas = DBTaS::getSingleTaS(getGetField("id"));
	$doku = unserialize($tas["t_dokumenty"]);
}

if(getPostField("add-id")) {
	if(DBDokumenty::getSingleDokument(getPostField("add-id"))) {
		$doku[] = getPostField("add-id");
	
		DBTaS::editTaS($tas["t_id"], $tas["t_jmeno"], $tas["t_kde"], $tas["t_info"], $tas["t_od"], $tas["t_do"],
			$tas["t_kapacita"], serialize($doku), $tas["t_kapacita"]);
		
		unset($_POST["add-id"]);
		$tas = DBTaS::getSingleTaS(getGetField("id"));
		$doku = unserialize($tas["t_dokumenty"]);
	}
}

include("files/Admin/TaSDokumenty/Display.inc");
return;
?>