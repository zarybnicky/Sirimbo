<?php
function formError() {
	echo "<span style=\"color:red;\"> !!!</span>\n";
}
function notice($msg) {
	echo "<div class=\"notice\">", $msg, "</div>\n";
}
function header_main($msg) {
	echo "<div class=\"h_section\">", $msg, "</div>\n";
}
function header_minor($msg) {
	echo "<div class=\"h_minor\">", $msg, "</div>\n";
}
function formatTime($str, $forDisplay) {
	if($forDisplay) {
		return substr($str, 0, 5);
	} else {
		return $str . ":00";
	}
}
function timeSubstract($first, $sec) {
	if(strcmp($first, $sec) > 0) {
		$tmp = $first;
		$first = $sec;
		$sec = $tmp;
	}
	
	list($f_hrs, $f_min) = explode(":", $first);
	list($s_hrs, $s_min) = explode(":", $sec);
	
	$m_diff = $f_min - $s_min;
	$h_diff = $f_hrs - $s_hrs;
	
	$r = abs($h_diff * 60 + $m_diff);
	
	return (floor($r / 60) . ":" . ($r % 60));
}
function timeAdd($first, $sec) {
	list($f_hrs, $f_min) = explode(":", $first);
	list($s_hrs, $s_min) = explode(":", $sec);
	
	$m = $f_min + $s_min;
	$h = floor($m / 60) + $f_hrs + $s_hrs;
	
	return ($h . ":" . ($m % 60));
}
function formatDate($str) {
	list($year, $month, $day) = explode("-", $str);
	return (int)$day . ". " . (int)$month . ". " . $year;
}
function checkDateString($date) {
	global $isConfirmation;
	if($isConfirmation) {
		if(preg_match("/^((?:19|20)\d\d)-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])$/", $date)) {
			list($year, $month, $day) = explode("-", $date);
			if(
				$day == 31 && ($month == 4 || $month == 6 || $month == 9 || $month == 11) ||
				$day >= 30 && $month == 2 ||
				$month == 2 && $day == 29 && !($year % 4 == 0 && ($year % 100 != 0 || $year % 400 == 0))
			) {
				formError();
				return true;
			} else {
				return false;
			}
		} else {
			formError();
			return true;
		}
	} else {
		return true;
	}
}
function checkPostField($regex, $text) {
	global $isConfirmation;
	if($isConfirmation) {
		if(isset($_POST[$text]) && preg_match($regex, $_POST[$text])) {	
			return false;
		} else {
			formError();
			return true;
		}
	} else {
		return true;
	}
}
function checkPostFieldLength($minIncl, $maxIncl, $text) {
	global $isConfirmation;
	if($isConfirmation) {
		if(isset($_POST[$text]) && strlen($_POST[$text]) >= $minIncl && strlen($_POST[$text]) <= $maxIncl) {	
			return false;
		} else {
			formError();
			return true;
		}
	} else {
		return true;
	}
}
function getPostField($field) {
	if(isset($_POST[$field]))
		return $_POST[$field];
	else
		return null;
}
function getGetField($field) {
	if(isset($_GET[$field]))
		return $_GET[$field];
	else
		return null;
}
function echoUsers($list_name, $list) {
	echo "<select name=\"", $list_name, "\">\n";
	echo "<option value=\"none\"";
	if(!getPostField($list_name))
		echo " selected=\"selected\"";
	echo ">Vyber si :o)</option>\n";
	$treneri = DBUser::getTrener();
	foreach($list as $item) {
		echo "<option value=\"" . $item["u_id"] . "\"";
		if(getPostField($list_name) == $item["u_id"])
			echo " selected=\"selected\"";
		echo ">" . $item["u_jmeno"] . " " . $item["u_prijmeni"];
		echo "</option>\n";
	}
	echo "</select>\n";
}
function echoDate($day_fieldname, $month_fieldname, $year_fieldname) {
	$selected = (int) getPostField($day_fieldname);
	echo "<select name=\"$day_fieldname\">\n";
	echo "<option value=\"00\">Den</option>\n";
	for($i = 1; $i < 32; $i++) {
		if($i < 10)
			echo "<option value=\"0", $i, "\"";
		else
			echo "<option value=\"", $i, "\"";
		if($selected == $i)
			echo " selected=\"selected\"";
		echo ">", $i, "</option>\n";
	}
	echo "</select>\n";
	
	$selected = (int) getPostField($month_fieldname);
	$months = array (1 => "Leden", "Únor", "Březen", "Duben", "Květen", "Červen", "Červenec",
		"Srpen", "Září", "Říjen", "Listopad", "Prosinec");
	echo "<select name=\"$month_fieldname\">\n";
	echo "<option value=\"00\">Měsíc</option>\n";
	foreach ($months as $num => $str) {
		if($num < 10)
			echo "<option value=\"0" . $num . "\"";
		else
			echo "<option value=\"" . $num . "\"";
		if ($selected == $num)
			echo " selected=\"selected\"";
		echo ">" . $str . "</option>\n";
	}
	echo "</select>\n";
	
	$selected = (int) substr(getPostField($year_fieldname), 2);
	echo "<select name=\"$year_fieldname\">\n";
	echo "<option value=\"0000\">Rok</option>\n";
	for($i = 10; $i < 21; $i++) {
		echo "<option value=\"20", $i, "\"";
		if ($selected == $i)
			echo " selected=\"selected\"";
		echo ">20", $i, "</option>\n";
	}
	echo "</select>\n";
}
?>