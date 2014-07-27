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
    require 'index2.php';
    return;
}
//end OFF switch*/

ini_set('session.use_trans_sid', 0);
ini_set('session.use_only_cookies', 1);

session_start();
session_regenerate_id();

require 'files/Core/settings.php';
require 'files/Core/form.php';
require 'files/Core/debug.php';
require 'files/Core/log.php';
require 'files/Core/request.php';
require 'files/Controller/Interface.php';
require 'files/Controller/Abstract.php';

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
        Helper::get()->redirect(
            '/member/profil/edit',
            'Prosím vyplňte požadované údaje.', true
        );
    }
}

$d = new Dispatcher();
$d->dispatch(Request::getLiteralURL(), Request::getAction(), Request::getID());
?>