<?php
/*/debug
if(stripos($_GET['file'], 'cookie_set') !== false) {
	setcookie('debug', '1', 0, '/');
	header('Location: /');
	return;
}
if(!isset($_COOKIE['debug'])) {
	include('index2.php');
	return;
}
//end_debug*/

session_start();
session_regenerate_id();

include("files/Core/settings.php");
include("files/Core/form.php");
include("files/Core/debug.php");
include("files/Controller/Interface.php");
include("files/Controller/Abstract.php");

define('TISK', (isset($_GET['view']) && $_GET['view'] == 'tisk') ? TRUE : FALSE);

if(TISK) {
	;
} elseif(!session('page_id')) {
	session('page_id', $_SERVER['REQUEST_URI']);
} elseif(!session('referer_id') || $_SERVER['REQUEST_URI'] != session('page_id')) {
	session('referer_id', session('page_id'));
	session('page_id', $_SERVER['REQUEST_URI']);
}

Request::setDefault('home');
Request::setURL(get('file'));
Request::setURI($_SERVER['REQUEST_URI']);
Request::setReferer(session('referer_id'));
unset($_GET['file']);

if(session('login') === null) {
	if(post('action') == 'login' || post('action') == 'enter') {
		post('pass', User::crypt(post('pass')));
	
		if(!User::login(post('login'), post('pass'))) {
			View::redirect('/login', 'Špatné jméno nebo heslo!', true);
		} elseif(get('return')) {
			View::redirect(get('return'));
		} else {
			View::redirect('/member/home');
		}
	}
} else {
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
?>