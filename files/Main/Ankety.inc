<?php
header_main('Ankety');

if(!empty($_POST)) {
	$ip = getIP();
	
	if(DBAnkety::isUniqueIP($_POST['id'], $ip)) {
		DBAnkety::vote($_POST['id'], $_POST['choice'], $ip);
		notice('Váš hlas byl uložen');
	} else {
		notice('Z vaší IP adresy už někdo hlasoval.');
	}
}

$result = DisplayAnkety::viewAnkety(false, true);
if(!$result)  {
	notice('Žádné ankety');
	return;
}
?>