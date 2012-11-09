<?php
header_main('Aktuálně');
notice(View::getRedirectMessage());

if(Request::getID() && ($data = DBAktuality::getSingleAktualita(Request::getID()))) {
	DisplayAktuality::viewClanek($data, false);
	return;
}

switch(Request::getAction()) {
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