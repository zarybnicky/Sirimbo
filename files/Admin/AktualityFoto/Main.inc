<?php
User::checkPermissionsError(L_EDITOR);
header_main("Správa aktualit");
notice(View::getRedirectMessage());

$id = Request::getID();
if(!$id || !($data = DBAktuality::getSingleAktualita($id)))
	View::redirect('/admin/aktuality', 'Článek s takovým ID neexistuje');

$items = DBAktuality::getAktualityFoto($id);

if(is_numeric(get('dir')))
	include('files/Admin/AktualityFoto/DisplayGalerie.inc');
else
	include('files/Admin/AktualityFoto/DisplayOverview.inc');
?>