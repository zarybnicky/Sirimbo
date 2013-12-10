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

//Project autoloader
require __DIR__ . DIRECTORY_SEPARATOR . 'src' . DIRECTORY_SEPARATOR . 'autoload.php';

//Project specific settings
require __DIR__ . DIRECTORY_SEPARATOR . 'config' . DIRECTORY_SEPARATOR . 'constants.php';
require __DIR__ . DIRECTORY_SEPARATOR . 'config' . DIRECTORY_SEPARATOR . 'db.php';

mb_internal_encoding('UTF-8');

date_default_timezone_set('Europe/Paris');

function errorHandler($severity, $message, $filepath, $line)
{
    if (TKOlomouc\Utility\Request::getURLStatic() == 'error') {
        TKOlomouc\Utility\Log::write("Recursive error message!");
        die('Fatal error: Rekurzivní smyčka přesměrování!');
    }
    TKOlomouc\Utility\Log::write("$filepath, line $line ($severity): $message");

    if ($severity & (E_STRICT | E_DEPRECATED | E_NOTICE)) {
        return true;
    }

    ob_end_clean();
    header('Location: /error?id=script_fatal');
    return true;
}
set_error_handler('errorHandler');

$request = new TKOlomouc\Utility\Request();

$request->setDefault('home');
$request->setURL($_SERVER['REQUEST_URI']);
$request->setReferer($request->session('referer_id'));
$request->setView(TISK ? 'plainhtml' : 'html');

if (TISK) {
    ;
} elseif (!$request->session('page_id')) {
    $request->session('page_id', $_SERVER['REQUEST_URI']);
} elseif (!session('referer_id') || $_SERVER['REQUEST_URI']
    != $request->session('page_id')
) {
    $request->session('referer_id', $request->session('page_id'));
    $request->session('page_id', $_SERVER['REQUEST_URI']);
}

if ($request->session('login') === null) {
    if ($request->post('action') == 'login'
        || $request->post('action') == 'enter'
    ) {
        $request->post('pass', TKOlomouc\Utility\User::crypt($request->post('pass')));

        if (!TKOlomouc\Utility\User::login($request->post('login'), $request->post('pass'))) {
            TKOlomouc\Utility\Response::redirect('/login', 'Špatné jméno nebo heslo!', true);
        } elseif ($request->get('return')) {
            TKOlomouc\Utility\Response::redirect($request->get('return'));
        } else {
            TKOlomouc\Utility\Response::redirect('/member/home');
        }
    }
} else {
    TKOlomouc\Utility\User::loadUser($request->session('id'));
    if ($request->session('invalid_data') === '1'
        && $request->getURL() !== 'member/profil/edit'
        && $request->getURL() !== 'logout'
    ) {
        TKOlomouc\Utility\Response::redirect(
            '/member/profil/edit',
            'Prosím vyplňte požadované údaje.', true
        );
    }
}

$d = new TKOlomouc\Utility\Dispatcher();
$d->dispatch($request);
