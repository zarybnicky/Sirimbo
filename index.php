<?php
use TKOlomouc;
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

session_start();
session_regenerate_id();


//PSR generic autoloader
require __DIR__ . DIRECTORY_SEPARATOR . 'lib'
    . DIRECTORY_SEPARATOR . 'Psr4Autoloader.php';

$autoloader = new \Psr\Autoload\Psr4Autoloader();

$autoloader->addNamespace(
    'TKOlomouc',
    __DIR__ . DIRECTORY_SEPARATOR . 'src' . DIRECTORY_SEPARATOR . 'library'
);
$autoloader->addNamespace(
    'TKOlomouc',
    __DIR__ . DIRECTORY_SEPARATOR . 'src' . DIRECTORY_SEPARATOR . 'application'
);

$autoloader->register();


//Twig-specific autoloader
require __DIR__ . DIRECTORY_SEPARATOR . 'lib'
    . DIRECTORY_SEPARATOR . 'Twig' . DIRECTORY_SEPARATOR . 'Autoloader.php';

Twig_Autoloader::register();


//Project specific settings
require __DIR__ . DIRECTORY_SEPARATOR . 'src' . DIRECTORY_SEPARATOR . 'application'
    . DIRECTORY_SEPARATOR . 'constants.php';
require SETTINGS . DIRECTORY_SEPARATOR . 'db.php';


ini_set('session.use_trans_sid', 0);
ini_set('session.use_only_cookies', 1);

mb_internal_encoding('UTF-8');

date_default_timezone_set('Europe/Paris');

function errorHandler($severity, $message, $filepath, $line) {
    if ($severity & (E_STRICT | E_DEPRECATED)) {
        return false;
    }
    ob_end_clean();
    if (Request::getURL() == 'error') {
        Log::write("Recursive error message!");
        die('Fatal error: Rekurzivní smyčka přesměrování!');
    }
    Log::write("$filepath, line $line ($severity): $message");
    header('Location: /error?id=script_fatal');
    return true;
}
set_error_handler('errorHandler');

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

if (session('login') === null) {
    if (post('action') == 'login' || post('action') == 'enter') {
        post('pass', TKOlomouc\Utility\User::crypt(post('pass')));

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

$d = new TKOlomouc\Utility\Dispatcher();
$d->dispatch(Request::getLiteralURL(), Request::getAction(), Request::getID());
?>