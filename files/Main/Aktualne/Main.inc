<?php
header_main('Aktuálně');

notice(View::getRedirectMessage());

$request = explode('/', $_SERVER['REQUEST_URI']);
$action = array_slice($request, 2);
if(empty($action)) $action[] = '';

if(is_numeric($action[0]) && ($data = DBAktuality::getSingleAktualita($action[0]))) {
	DisplayAktuality::viewClanek($data, false);
	return;
}

switch($action[0]) {
	case '':
	case 'posledni':
		header_minor('Nejnovější články');
		$result = DisplayAktuality::viewAktuality();
		if(!$result)  {
			notice('Žádné články');
		}
		return;
	case 'videa':
		header_minor('Videa');
		$result = DisplayAktuality::viewAktuality(AKTUALITY_VIDEA);
		if(!$result)  {
			notice('Žádné články');
		}
		return;
	case 'clanky':
		header_minor('Články');
		$result = DisplayAktuality::viewAktuality(AKTUALITY_CLANKY);
		if(!$result)  {
			notice('Žádné články');
		}
		return;
	case 'kratke-zpravy':
		header_minor('Krátké zprávy');
		$result = DisplayAktuality::viewAktuality(AKTUALITY_KRATKE);
		if(!$result)  {
			notice('Žádné články');
		}
		return;
}
?> 