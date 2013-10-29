<?php
/*/sitewide OFF switch
if (isset($_GET['file'])) {
    if (stripos($_GET['file'], 'cookie_set') !== false) {
        setcookie('off_mode', '1', 0, '/');
        header('Location: /');
        return;
    }
    if (stripos($_GET['file'], 'cookie_reset') !== false) {
        setcookie('off_mode', false, 0, '/');
        header('Location: /');
        return;
    }
}
if (!isset($_COOKIE['off_mode'])) {
    include 'index2.php';
    return;
}
//end OFF switch*/

session_start();
session_regenerate_id();

include 'files/Core/settings.php';
include 'files/Core/form.php';
include 'files/Core/debug.php';
include 'files/Core/log.php';
include 'files/Core/request.php';
include 'files/Controller/Interface.php';
include 'files/Controller/Abstract.php';

define('TISK', (isset($_GET['view']) && $_GET['view'] == 'tisk') ? true : false);

if (TISK) {
    ;
} elseif (!session('page_id')) {
    session('page_id', $_SERVER['REQUEST_URI']);
} elseif (!session('referer_id') || $_SERVER['REQUEST_URI'] != session('page_id')) {
    session('referer_id', session('page_id'));
    session('page_id', $_SERVER['REQUEST_URI']);
}

Request::setDefault('home');
Request::setURL(get('file'));
Request::setURI($_SERVER['REQUEST_URI']);
Request::setReferer(session('referer_id'));
unset($_GET['file']);

if (session('login') === null) {
    if (post('action') == 'login' || post('action') == 'enter') {
        post('pass', User::crypt(post('pass')));

        if (!User::login(post('login'), post('pass'))) {
            Helper::get()->redirect('/login', 'Špatné jméno nebo heslo!', true);
        } elseif (get('return')) {
            Helper::get()->redirect(get('return'));
        } else {
            Helper::get()->redirect('/member/home');
        }
    }
} else {
    User::loadUser(session('id'));
    if (session('invalid_data') === '1'
        && Request::getURL() !== 'member/profil/edit'
        && Request::getURL() !== 'logout'
    ) {
        Helper::get()->redirect('/member/profil/edit', 'Prosím vyplňte požadované údaje.', true);
    }
}

$d = new Dispatcher();
$d->dispatch(Request::getLiteralURL(), Request::getAction(), Request::getID());
?>