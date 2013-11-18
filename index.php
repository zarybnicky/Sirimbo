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

session_start();
session_regenerate_id();


//PSR generic autoloader
require __DIR__ . DIRECTORY_SEPARATOR . 'src' . DIRECTORY_SEPARATOR . 'autoload.php';

$autoloader = new Psr4Autoloader();

$autoloader->addNamespace(
    'TKOlomouc',
    __DIR__ . DIRECTORY_SEPARATOR . 'src' . DIRECTORY_SEPARATOR . 'library'
);
$autoloader->addNamespace(
    'TKOlomouc',
    __DIR__ . DIRECTORY_SEPARATOR . 'src' . DIRECTORY_SEPARATOR . 'application'
);
$autoloader->register();


//Composer autoloader
require 'vendor' . DIRECTORY_SEPARATOR . 'autoload.php';


//Project specific settings
require __DIR__ . DIRECTORY_SEPARATOR . 'config' . DIRECTORY_SEPARATOR . 'constants.php';
require __DIR__ . DIRECTORY_SEPARATOR . 'config' . DIRECTORY_SEPARATOR . 'db.php';

ini_set('session.use_trans_sid', 0);
ini_set('session.use_only_cookies', 1);

mb_internal_encoding('UTF-8');

date_default_timezone_set('Europe/Paris');

function errorHandler($severity, $message, $filepath, $line) {
    //if ($severity & (E_STRICT | E_DEPRECATED)) {
    //    return false;
    //}
    ob_end_clean();
    if (TKOlomouc\Utility\Request::getURL() == 'error') {
        TKOlomouc\Utility\Log::write("Recursive error message!");
        die('Fatal error: Rekurzivní smyčka přesměrování!');
    }
    TKOlomouc\Utility\Log::write("$filepath, line $line ($severity): $message");
    header('Location: /error?id=script_fatal');
    return true;
}
set_error_handler('errorHandler');

if (TISK) {
    ;
} elseif (!TKOlomouc\Utility\Miscellaneous::session('page_id')) {
    TKOlomouc\Utility\Miscellaneous::session('page_id', $_SERVER['REQUEST_URI']);
} elseif (!session('referer_id') || $_SERVER['REQUEST_URI']
    != TKOlomouc\Utility\Miscellaneous::session('page_id')
) {
    TKOlomouc\Utility\Miscellaneous::session('referer_id', TKOlomouc\Utility\Miscellaneous::session('page_id'));
    TKOlomouc\Utility\Miscellaneous::session('page_id', $_SERVER['REQUEST_URI']);
}

TKOlomouc\Utility\Request::setDefault('home');
TKOlomouc\Utility\Request::setURL($_SERVER['REQUEST_URI']);
TKOlomouc\Utility\Request::setReferer(TKOlomouc\Utility\Miscellaneous::session('referer_id'));

if (TKOlomouc\Utility\Miscellaneous::session('login') === null) {
    if (TKOlomouc\Utility\Miscellaneous::post('action') == 'login'
        || TKOlomouc\Utility\Miscellaneous::post('action') == 'enter'
    ) {
        TKOlomouc\Utility\Miscellaneous::post('pass', TKOlomouc\Utility\User::crypt(TKOlomouc\Utility\Miscellaneous::post('pass')));

        if (!TKOlomouc\Utility\User::login(TKOlomouc\Utility\Miscellaneous::post('login'), TKOlomouc\Utility\Miscellaneous::post('pass'))) {
            TKOlomouc\Utility\Response::redirect('/login', 'Špatné jméno nebo heslo!', true);
        } elseif (TKOlomouc\Utility\Miscellaneous::get('return')) {
            TKOlomouc\Utility\Response::redirect(TKOlomouc\Utility\Miscellaneous::get('return'));
        } else {
            TKOlomouc\Utility\Response::redirect('/member/home');
        }
    }
} else {
    TKOlomouc\Utility\User::loadUser(TKOlomouc\Utility\Miscellaneous::session('id'));
    if (TKOlomouc\Utility\Miscellaneous::session('invalid_data') === '1'
        && TKOlomouc\Utility\Request::getURL() !== 'member/profil/edit'
        && TKOlomouc\Utility\Request::getURL() !== 'logout'
    ) {
        TKOlomouc\Utility\Response::redirect(
            '/member/profil/edit',
            'Prosím vyplňte požadované údaje.', true
        );
    }
}

$d = new TKOlomouc\Utility\Dispatcher();
$d->dispatch(
    TKOlomouc\Utility\Request::getLiteralURL(),
    TKOlomouc\Utility\Request::getAction(),
    TKOlomouc\Utility\Request::getID()
);
