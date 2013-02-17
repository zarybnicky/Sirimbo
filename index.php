<?php
include("files/Core/settings.php");
include("files/Core/log.php");
include("files/Core/form.php");
include("files/Core/cache.php");
include("files/Core/database.php");
include("files/Core/database/dbuser.php");
include("files/Core/view.php");
include("files/Core/request.php");
include("files/Core/user.php");
include("files/Core/permissions.php");
include('files/Core/helper.php');
include("files/Core/paging.php");
include("files/Core/mailer.php");
include("files/Core/dispatcher.php");
include("files/Controller/Interface.php");
include("files/Core/debug.php");		//DEBUG ONLY!!!

define('TISK', (isset($_GET['view']) && $_GET['view'] == 'tisk') ? TRUE : FALSE);

session_start();
session_regenerate_id();

//Are all CORE vars present?
if(!isset($sitemap_static) || !isset($sitemap_dynamic) || !class_exists("Database") ||
		!class_exists("DBUser") || !class_exists("User") || !class_exists("View") || !class_exists("Log") ||
		!class_exists("Permissions") || !class_exists("Mailer") || !class_exists("Request")) {
	if(class_exists("View")) {
		View::viewDynamic("files/Error/KeyFileCorrupt.inc");
		die();
	} else {
		die("Poškozené knihovní soubory");
	}
}

Request::setURL(get('file'));
Request::setURI($_SERVER['REQUEST_URI']);
$file = Request::getLiteralURL('home');
unset($_GET['file']);

if(TISK) {
	;
} elseif(!session('page_id')) {
	session('page_id', $_SERVER['REQUEST_URI']);
} elseif(!session('referer_id') || $_SERVER['REQUEST_URI'] != session('page_id')) {
	session('referer_id', session('page_id'));
	session('page_id', $_SERVER['REQUEST_URI']);
}
Request::setReferer(session('referer_id'));

if(session('login')) {
	User::loadUser(session('id'));
	if(session('invalid_data') &&
			Request::getURL() !== 'member/profil/edit' && Request::getURL() !== 'logout')
		View::redirect('/member/profil/edit', 'Prosím vyplňte požadované údaje.', true);
}

ob_start();
ob_start();

$d = new Dispatcher();
$d->dispatch(Request::getLiteralURL(), Request::getAction(), Request::getID());

$main = ob_get_clean();

include(TISK ? HEADER_TISK : HEADER);
echo $main;
include(TISK ? FOOTER_TISK : FOOTER);

ob_end_flush();
exit;

/*if(array_key_exists($file, $sitemap_static)) {
	$file = $sitemap_static[$file];
	View::viewStatic($file);
} elseif(array_key_exists($file, $sitemap_dynamic)) {
	$file = $sitemap_dynamic[$file];
	if(file_exists($file))
		View::viewDynamic($file);
	elseif(isset($sitemap_dynamic[$file]))
		View::redirect('/' . $file);
	else
		View::viewError(ER_NOT_FOUND_RIGHT);
} else {
	View::viewStatic("files/Error/NotPossible.inc");
}*/
?>