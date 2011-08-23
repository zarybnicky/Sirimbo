<?php
function formError() {
	echo "<span style=\"color:red;\"> !!!</span>";
}
function notice($msg) {
	echo "<div class=\"notice\">", $msg, "</div>";
}
function header_main($msg) {
	echo "<div class=\"h_section\">", $msg, "</div>";
}
function header_minor($msg) {
	echo "<div class=\"h_minor\">", $msg, "</div>";
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
?>