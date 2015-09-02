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

ob_start();
session_start();
session_regenerate_id();

require 'files/autoload.php';
require 'files/Core/settings.php';
require 'files/Core/utils.php';
require 'files/Core/debug.php';
require 'files/Core/log.php';
require 'files/Core/request.php';
require 'files/Controller/Interface.php';
require 'files/Controller/Abstract.php';

$request = new Request(
    $_SERVER['REQUEST_URI'],
    $_SERVER['REQUEST_METHOD'],
    getallheaders(),
    $_SERVER,
    $_COOKIE,
    $_GET,
    $_POST,
    $_FILES,
    $_SESSION
);
$_SERVER = $_COOKIE = $_GET = $_POST = $_FILES = array();
$request->setDefault('home');
$request->setReferer($request->session('referer_id'));

Database::setRequest($request);
Log::setRequest($request);

define('TISK', ($request->get('view') == 'tisk') ? true : false);

if (TISK) {
    ;
} elseif (!$request->session('page_id')) {
    $request->session('page_id', $request->server('REQUEST_URI'));
} elseif (
    !$request->session('referer_id') ||
    $request->server('REQUEST_URI') != $request->session('page_id')
) {
    $request->session('referer_id', $request->session('page_id'));
    $request->session('page_id', $request->server('REQUEST_URI'));
}

try {
    if ($request->session('login') !== null) {
        User::loadUser($request->session('id'));
        if ($request->session('invalid_data') === '1'
            && $request->getURI() !== 'member/profil/edit'
            && $request->getURI() !== 'logout'
        ) {
            Helper::instance()->redirect(
                '/member/profil/edit',
                'Prosím vyplňte požadované údaje.',
                true
            );
        }
    } elseif ($request->post('login') && $request->post('pass')) {
        $request->post('pass', User::crypt($request->post('pass')));

        if (!User::login($request->post('login'), $request->post('pass'))) {
            Helper::instance()->redirect('/login', 'Špatné jméno nebo heslo!', true);
        } elseif ($request->get('return')) {
            Helper::instance()->redirect($request->get('return'));
        } else {
            Helper::instance()->redirect('/member/nastenka');
        }
    }

    $d = new Dispatcher();
    $d->dispatch($request);
} catch (ViewException $e) {
    Log::write(
        $e->getMessage()
        . '(' . $e->getFile() . ':' . $e->getLine() . ")\n"
        . $e->getTraceAsString()
    );
    ob_clean();
    Helper::instance()->redirect('/error?id=' . $e->getErrorFile());
} catch (Exception $e) {
    Log::write(
        $e->getMessage()
        . ' (' . $e->getFile() . ':' . $e->getLine() . ")\n"
        . $e->getTraceAsString()
    );
    ob_clean();
    Helper::instance()->redirect('/error?id=' . (new ViewException(''))->getErrorFile());
}
