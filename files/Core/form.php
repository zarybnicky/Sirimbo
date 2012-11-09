<?php
class Form {
	private $valid;
	private $messages;
	private $fields;
	
	const REGEXP_DATE = '/^((?:19|20)\d\d)-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])$/i';
	const REGEXP_EMAIL = '//i';
	const REGEXP_PHONE = '//i';
	
	function __construct() {
		$this->valid = true;
		$this->messages = array();
		$this->fields = array();
	}
	function isValid() {
		return $this->valid;
	}
	function getMessages() {
		return $this->messages;
	}
	function getFields() {
		return $this->fields;
	}
	function checkDate($i, $message, $name = '') {
		if(preg_match(Form::REGEXP_DATE, $i)) {
			list($year, $month, $day) = explode('-', $i);
			if(!(
				$day == 31 && ($month == 4 || $month == 6 || $month == 9 || $month == 11) ||
				$day >= 30 && $month == 2 ||
				$month == 2 && $day == 29 && !($year % 4 == 0 && ($year % 100 != 0 || $year % 400 == 0))
			)) {
				return true;
			}
		}
		$this->error($message, $name);
		return false;
	}
	function checkEmail($i, $message, $name = '') {
		return $this->checkRegexp(Form::REGEXP_EMAIL, $i, $message, $name);
	}
	function checkPhone($i, $message, $name = '') {
		return $this->checkRegexp(Form::REGEXP_PHONE, $i, $message, $name);
	}
	function checkRegexp($i, $regexp, $message, $name = '') {
		if(preg_match($regexp, $i))
			return true;
		
		$this->error($message, $name);
		return false;
	}
	function checkLength($i, $min, $max, $message, $name = '') {
		$len = strlen($i);
		if($len >= $min && $len <= $max)
			return true;
		
		$this->error($message, $name);
		return false;
	}
	function checkInArray($i, $array, $message, $name = '') {
		if(in_array($i, $array))
			return true;
		
		$this->error($message, $name);
		return false;
	}
	function checkNotEmpty($i, $message, $name = '') {
		if($i || $i === 0)
			return true;
		
		$this->error($message, $name);
		return false;
	}
	function checkNumeric($i, $message, $name = '') {
		if(is_numeric($i))
			return true;
		
		$this->error($message, $name);
		return false;
	}
	private function error($message, $name = '') {
		$this->valid = false;
		$this->messages[] = $message;
		if($name !== '')
			$this->fields[] = $name;
	}
}

function formError() {
	echo '<span style="color:red;"> !!!</span>', "\n";
}
function notice($text, $return = false) {
	if(!$text) return;
	
	if(!$return)echo '<div class="notice">', $text, '</div>', "\n";
	else		return '<div class="notice">' . $text . '</div>' . "\n";
}
function header_main($text, $return = false) {
	if(!$text) return;
	
	if(!$return)echo '<div class="h_section">', $text, '</div>', "\n";
	else		return '<div class="h_section">' . $text . '</div>' . "\n";
}
function header_minor($text, $return = false) {
	if(!$text) return;
	
	if(!$return)echo '<div class="h_minor">', $text, '</div>', "\n";
	else		return '<div class="h_minor">' . $text . '</div>' . "\n";
}
function getColorBox($color, $popis) {
	return '<div class="box" title="' . $popis . '" ' .
		'style="background-color:' . Settings::$barvy[$color][1] . '"></div>';
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
function getPostField($field = NULL, $value = NULL) {
	return post($field, $value);
}
function getGetField($field = NULL, $value = NULL) {
	return get($field, $value);
}
function post($field = NULL, $value = NULL) {
	if($field === NULL) {
		return $_POST;
	}
	
	if($value !== NULL) {
		$_POST[$field] = $value;
		return;
	}
	
	if(isset($_POST[$field]))
		return $_POST[$field];
	else
		return null;
}
function get($field = NULL, $value = NULL) {
	if($field === NULL) {
		return $_GET;
	}
	
	if($value !== NULL) {
		$_GET[$field] = $value;
		return;
	}
	
	if(isset($_GET[$field]))
		return $_GET[$field];
	else
		return null;
}
function session($field = NULL, $value = NULL) {
	if($field === NULL) {
		return $_SESSION;
	}
	
	if($value !== NULL) {
		$_SESSION[$field] = $value;
		return;
	}
	
	if(isset($_SESSION[$field]))
		return $_SESSION[$field];
	else
		return null;
}
function formatTime($str, $forDisplay) {
	if($forDisplay) {
		return substr($str, 0, 5); //15:00:00
	} else {
		return $str . ':00';
	}
}
function formatDate($str) {
	list($year, $month, $day) = explode('-', $str);
	return (int)$day . '. ' . (int)$month . '. ' . $year;
}
function formatTimestamp($str) {
	list($date, $time) = explode(' ', $str);
	$date = formatDate($date);
	$time = formatTime($time, 1);
	return implode(' ', array($date, $time));
}
function timeSubstract($first, $sec) {
	if(strcmp($first, $sec) > 0) {
		$tmp = $first;
		$first = $sec;
		$sec = $tmp;
	}
	
	list($f_hrs, $f_min) = explode(':', $first);
	list($s_hrs, $s_min) = explode(':', $sec);
	
	$m_diff = $f_min - $s_min;
	$h_diff = $f_hrs - $s_hrs;
	
	$r = abs($h_diff * 60 + $m_diff);
	
	return (floor($r / 60) . ':' . ($r % 60));
}
function timeAdd($first, $sec) {
	list($f_hrs, $f_min) = explode(':', $first);
	list($s_hrs, $s_min) = explode(':', $sec);
	
	$m = $f_min + $s_min;
	$h = floor($m / 60) + $f_hrs + $s_hrs;
	
	return ($h . ':' . ($m % 60));
}
function checkDateString($date) {
	global $isConfirmation;
	if($isConfirmation) {
		if(preg_match('/^((?:19|20)\d\d)-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])$/', $date)) {
			list($year, $month, $day) = explode('-', $date);
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
function echoDateSelect($day_name, $month_name, $year_name, $fromYear = 2010, $toString = 0) {
	$out = '';
	$s = Helper::get()->select()->get(false);
	
	$s->name($day_name)
		->value(post($day_name) ? post($day_name) : date('d'))
		->options(array(), true)
		->option('00', 'Den');
	for($i = 1; $i < 32; $i++)
		$s->option((($i < 10) ? ('0' . $i) : $i), $i);
	$out .= $s;
	
	$out .= $s->name($month_name)
		->value(post($month_name) ? post($month_name) : date('m'))
		->options(array(), true)
		->option('00', 'Měsíc')
		->option('01', 'Leden')		->option('02', 'Únor')
		->option('03', 'Březen')	->option('04', 'Duben')
		->option('05', 'Květen')	->option('06', 'Červen')
		->option('07', 'Červenec')	->option('08', 'Srpen')
		->option('09', 'Září')		->option('10', 'Říjen')
		->option('11', 'Listopad')	->option('12', 'Prosinec');
	
	$s->name($year_name)
		->value(post($year_name) ? post($year_name) : date('Y'))
		->options(array(), true)
		->option('0000', 'Rok');
	$toYear = ((int) date('Y')) + 5;
	for($i = $fromYear; $i < $toYear; $i++)
		$s->option($i, $i);
	$out .= $s;
	
	if($toString)
		return $out;
	else
		echo $out;
}
function echoTaborDokumenty($list_name, $kats) {
	echo '<select name="', $list_name, '">', "\n";
	echo '<option value="none"';
	if(!post($list_name))
		echo ' selected="selected"';
	echo '>Vyber si :o)</option>', "\n";
	if(is_array($kats)) { 
		foreach($kats as $kat) {
			$doku = DBDokumenty::getDokumentyByKategorie($kat);
			
			foreach($doku as $item) {
				echo '<option value="', $item['d_id'], '"';
				if(post($list_name) == $item['d_id'])
					echo ' selected="selected"';
				echo '>', $item['d_name'], ' (', $item['d_kategorie'], ')';
				echo '</option>', "\n";
			}
		}
	} else {
		$doku = DBDokumenty::getDokumentyByKategorie($kats);
		
		foreach($doku as $item) {
			echo '<option value="', $item['d_id'], '"';
			if(post($list_name) == $item['d_id'])
				echo ' selected="selected"';
			echo '>', $item['d_name'], ' (', Settings::$document_types[$item['d_kategorie']], ')';
			echo '</option>', "\n";
		}
	}
	echo '</select>', "\n";
}
function getCheckbox($name, $value = '', $default = false, $get = false, $readonly = false) {
	if($value === '')
		$value = $name;
	$checked = (($get == true) ?
		((get($name) != false) ? true : false) :
		((post($name) != false) ? true : false));
	if(!$checked)
		$checked = (bool) $default;
	return '<input type="checkbox" name="' . $name . '" value="' . $value . '"' .
		($checked ? ' checked="checked"' : '') . ($readonly ? ' readonly="readonly"' : '') . '/>';
}
function getRadio($name, $value, $default = false, $get = false, $readonly = false) {
	$checked = (($get == true) ?
		((get($name) == $value) ? true : false) :
		((post($name) == $value) ? true : false));
	if(!$checked)
		$checked = (bool) $default;
	return '<input type="radio" name="' . $name . '" value="' . $value . '"' .
		($checked ? ' checked="checked"' : '') . ($readonly ? ' readonly="readonly"' : '') . '/>';
}
function getReturnURI($default) {
	return post('referer') ? post('referer') : $default;
}
function getReturnInput() {
	return '<input type="hidden" name="referer" value="' . Request::getReferer() . '" />' . "\n";
}
function echoFullJmeno($userData) {
	if(is_array($userData)) {
		echo $userData['u_jmeno'], ' ',  $userData['u_prijmeni'];
	}
}
function getIP() {
	if(!empty($_SERVER['HTTP_CLIENT_IP'])) {
		return $_SERVER['HTTP_CLIENT_IP'];
	} elseif(!empty($_SERVER['HTTP_X_FORWARDED_FOR'])) {
		return $_SERVER['HTTP_X_FORWARDED_FOR'];
	} else {
		return $_SERVER['REMOTE_ADDR'];
	}
}
?>