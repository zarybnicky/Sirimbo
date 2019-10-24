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

require 'vendor/autoload.php';
require 'files/Core/settings.php';

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
$request->setReferer($request->server('HTTP_REFERER'));

Database::setRequest($request);
Log::setRequest($request);
Permissions::setRequest($request);

try {
    if ($request->session('login') !== null) {
        User::loadUser($request->session('id'));
        if ($request->session('invalid_data') === '1'
            && $request->getURI() !== 'member/profil/edit'
            && $request->getURI() !== 'logout'
        ) {
            (new RedirectHelper())->redirect(
                '/member/profil/edit',
                'Prosím vyplňte požadované údaje.',
                true
            );
        }
    } elseif ($request->post('login') && $request->post('pass')) {
        $request->post('pass', User::crypt($request->post('pass')));

        if (!User::login($request->post('login'), $request->post('pass'))) {
            (new RedirectHelper())->redirect('/login', 'Špatné jméno nebo heslo!');
        } elseif ($request->get('return')) {
            (new RedirectHelper())->redirect($request->get('return'));
        } else {
            (new RedirectHelper())->redirect('/member/home');
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
    (new RedirectHelper())->redirect('/error?id=' . $e->getErrorFile());
} catch (Exception $e) {
    Log::write(
        $e->getMessage()
        . ' (' . $e->getFile() . ':' . $e->getLine() . ")\n"
        . $e->getTraceAsString()
    );
    ob_clean();
    (new RedirectHelper())->redirect('/error?id=' . (new ViewException(''))->getErrorFile());
}
