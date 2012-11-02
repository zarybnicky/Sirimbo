<?php
include("files/Core/settings.php");
include("files/Core/log.php");
include("files/Core/form.php");
include("files/Core/cache.php");
include("files/Core/database.php");
include("files/Core/database/dbuser.php");
include("files/Core/user.php");
include("files/Core/view.php");
include("files/Core/request.php");
include("files/Core/permissions.php");
include('files/Core/helper.php');
include("files/Core/mailer.php");
include("files/Core/debug.php");		//DEBUG ONLY!!!

//Are all CORE vars present?
if(!$sitemap_static || !$sitemap_dynamic || !class_exists("Database") || !class_exists("DBUser") ||
		!class_exists("User") || !class_exists("View") || !class_exists("Log") ||
		!class_exists("Permissions") || !class_exists("Mailer") || !class_exists("Request")) {
	if(class_exists("View")) {
		View::viewDynamic("files/Error/KeyFileCorrupt.inc");
		die();
	} else {
		die("Poškozené knihovní soubory");
	}
}
define('TISK', (get('view') == 'tisk') ? TRUE : FALSE);

Request::setURL(get('file'));
Request::setURI($_SERVER['REQUEST_URI']);
$file = Request::getLiteralURL('home');

if(TISK) {
	;
} elseif(!session('page_id')) {
	session('page_id', $_SERVER['REQUEST_URI']);
} elseif(!session('referer_id') || $_SERVER['REQUEST_URI'] != session('page_id')) {
	session('referer_id', session('page_id'));
	session('page_id', $_SERVER['REQUEST_URI']);
}
Request::setReferer(session('referer_id'));

if(array_key_exists($file, $sitemap_static)) {
	$file = $sitemap_static[$file];
	View::viewStatic($file);
} else if(array_key_exists($file, $sitemap_dynamic)) {
	$file = $sitemap_dynamic[$file];
	View::viewDynamic($file);
} else {
	View::viewStatic("files/Error/NotPossible.inc");
}
?>