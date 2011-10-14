<?php
User::checkPermissionsError(L_TRENER);

if(!is_numeric(getGetField("id")) && getGetField("id") > 0 && getGetField("id") < 2147483647)
	View::viewError(ER_CORRUPT_DATA);
if(!($roz = DBRozpis::getSingleRozpis(getGetField("id"))))
	View::viewError(ER_CORRUPT_DATA);
$items = DBRozpis::getRozpisItem(getGetField("id"));
$users = DBUser::getActiveUsers();

header_main("Správa rozpisů");

if(empty($_POST)) {
	include("files/Admin/RozpisDetail/Display.inc");
	return;
}

if(getPostField("generate") == "overlap") {
	$end = "00:00";
	foreach($items as $item) {
		$l_start = formatTime($item["ri_od"], 1);
		$l_end = formatTime($item["ri_do"], 1);	
		$length = timeSubstract($l_end, $l_start);
		
		if(strcmp($end, $l_start) > 0) {
			$l_start = $end;
			$l_end = timeAdd($l_start, $length);
		}
		if(strcmp($l_start, $l_end) > 0)
			$l_end = timeAdd($l_start, $length);
		
		DBRozpis::editRozpisItem($item["ri_id"], $item["ri_partner"],
			formatTime($l_start, 0), formatTime($l_end, 0));
		
		$end = $l_end;
	}
	
	$items = DBRozpis::getRozpisItem(getGetField("id"));
} elseif(getPostField("generate") == "gen_add" &&
		is_numeric(getPostField("gen_add_hod")) &&
		is_numeric(getPostField("gen_add_len")) &&
		preg_match("/^[0-9]{2}:[0-9]{2}$/", getPostField("gen_add_od"))) {
	echo "Adding...";
	
	$od = getPostField("gen_add_od");
	$len = floor(getPostField("gen_add_len") / 60) . ":" .
		(int)(getPostField("gen_add_len") % 60);
	$do = timeAdd($od, $len);
	
	for($i = 0; $i < getPostField("gen_add_hod"); $i++) {
		DBRozpis::addRozpisItem(getGetField("id"), "0", formatTime($od, 0),
			formatTime($do, 0), "0");
		$od = $do;
		$do = timeAdd($od, $len);
	}
	
	$items = DBRozpis::getRozpisItem(getGetField("id"));
} else {
	if(getPostField("remove") > 0) {
		DBRozpis::removeRozpisItem(getPostField("remove"));
		$items = DBRozpis::getRozpisItem(getGetField("id"));
	}
	foreach($items as $item) {
		if(!(getPostField($item["ri_id"] . "-partner") == $item["ri_partner"] ||
				(getPostField($item["ri_id"] . "-partner") == "none" && $item["ri_partner"] =="0")) ||
				getPostField($item["ri_id"] . "-od") != formatTime($item["ri_od"], 1) ||
				getPostField($item["ri_id"] . "-do") != formatTime($item["ri_do"], 1) ||
				(bool) getPostField($item["ri_id"] . "-lock") != (bool) $item["ri_lock"]) {
			
			$partner = (getPostField($item["ri_id"] . "-partner") == "none") ? "0" :
				getPostField($item["ri_id"] . "-partner");
			$od = getPostField($item["ri_id"] . "-od");
			$do = getPostField($item["ri_id"] . "-do");
			$lock = getPostField($item["ri_id"] . "-lock") ? 1 : 0;
			
			if(preg_match("/^[0-9]{2}:[0-9]{2}$/", $od) &&
					preg_match("/^[0-9]{2}:[0-9]{2}$/", $do)) {
				DBRozpis::editRozpisItem($item["ri_id"], $partner, formatTime($od, 0), formatTime($do, 0), $lock);
			}
		}
	}
	$items = DBRozpis::getRozpisItem(getGetField("id"));
	
	if(is_numeric(getPostField("add_partner")) || getPostField("add_partner") == "none") {
		$partner = (getPostField("add_partner") == "none") ? "0" : getPostField("add_partner");
		$od = getPostField("add_od");
		$do = getPostField("add_do");
		$lock = getPostField("add_lock");
		
		if(preg_match("/^[012]*[0-9]{1}:[0-9]{2}$/", $od) &&
				preg_match("/^[012]*[0-9]{1}:[0-9]{2}$/", $do)) {
			DBRozpis::addRozpisItem(getGetField("id"), $partner, formatTime($od, 0), formatTime($do, 0), $lock);
			unset($_POST["add_partner"]);
			$items = DBRozpis::getRozpisItem(getGetField("id"));
		}
	}
}

include("files/Admin/RozpisDetail/Display.inc");
return;
?>
