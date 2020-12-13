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

require 'vendor/autoload.php';
require 'config.php';
require 'files/Core/settings.php';
require 'files/Core/utils.php';

ob_start();
ini_set('session.use_trans_sid', 0);
ini_set('session.use_only_cookies', 1);
ini_set('session.cookie_lifetime', 0);
ini_set('session.cookie_httponly', 1);
ini_set('session.serialize_handler', 'php_serialize');

register_shutdown_function('shutdownHandler');
set_error_handler('errorHandler');
error_reporting(-1);

$request = new Request($_SERVER['REQUEST_URI'], $_SERVER['REQUEST_METHOD'], $_GET, $_POST);
$request->setDefault('home');
$request->setReferer($_SERVER['HTTP_REFERER'] ?? getallheaders()['Referer'] ?? null);

session_set_save_handler(new DbSessionHandler(), true);
session_start();

try {
    if (isset($_SESSION['login']) && $_SESSION['login'] !== null) {
        Session::loadUser($_SESSION['id']);
        if ($_SESSION['gdpr']) {
            if (!in_array($request->getURI(), ['member/profil/gdpr', 'logout'])) {
                return (new RedirectHelper())->redirect('/member/profil/gdpr');
            }
        } elseif ($_SESSION['invalid']) {
            if (!in_array($request->getURI(), ['member/profil/edit', 'logout'])) {
                return (new RedirectHelper())->redirect('/member/profil/edit', 'Prosím vyplňte požadované údaje.');
            }
        }
    } elseif ($request->post('login') && $request->post('pass')) {
        $request->post('pass', User::crypt($request->post('pass')));

        if (!Session::login($request->post('login'), $request->post('pass'))) {
            (new RedirectHelper())->redirect('/login', 'Špatné jméno nebo heslo!');
        } elseif ($request->get('return')) {
            (new RedirectHelper())->redirect($request->get('return'));
        } else {
            (new RedirectHelper())->redirect('/member/home');
        }
    }

    $d = new Dispatcher();
    $d->dispatch($request);
} catch (AuthorizationException $e) {
    ob_clean();
    (new RedirectHelper())->redirect('/error?id=' . $e->getErrorFile());
} catch (ViewException $e) {
    fwrite(
        fopen('php://stderr', 'w'),
        $_SERVER['REQUEST_URI'] . ": {$e->getMessage()}\n"
        . '(' . $e->getFile() . ':' . $e->getLine() . ")\n"
        . $e->getTraceAsString()
        . "\tPOST: " . json_encode($_POST) . "\n"
        . "\tSESSION: " . json_encode($_SESSION) . "\n"
    );
    ob_clean();
    (new RedirectHelper())->redirect('/error?id=' . $e->getErrorFile());
} catch (Exception $e) {
    fwrite(
        fopen('php://stderr', 'w'),
        $_SERVER['REQUEST_URI'] . ": {$e->getMessage()}\n"
        . '(' . $e->getFile() . ':' . $e->getLine() . ")\n"
        . $e->getTraceAsString()
        . "\tPOST: " . json_encode($_POST) . "\n"
        . "\tSESSION: " . json_encode($_SESSION) . "\n"
    );
    ob_clean();
    (new RedirectHelper())->redirect('/error?id=' . (new ViewException(''))->getErrorFile());
}
