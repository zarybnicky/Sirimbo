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
                return new RedirectHelper('/member/profil/gdpr');
            }
        } elseif ($_SESSION['invalid']) {
            if (!in_array($request->getURI(), ['member/profil/edit', 'logout'])) {
                return new RedirectHelper('/member/profil/edit', 'Prosím vyplňte požadované údaje.');
            }
        }
    } elseif ($_POST['login'] && $_POST['pass']) {
        $_POST['pass'] = User::crypt($_POST['pass']);

        if (!Session::login($_POST['login'], $_POST['pass'])) {
            new RedirectHelper('/login', 'Špatné jméno nebo heslo!');
        } elseif ($_GET['return']) {
            new RedirectHelper($_GET['return']);
        } else {
            new RedirectHelper('/member');
        }
    }

    $router = makeRouter($request);
    $router->dispatchGlobal();
} catch (AuthorizationException $e) {
    ob_clean();
    new RedirectHelper('/error?id=' . $e->getErrorFile());
} catch (NotFoundException $e) {
    ob_clean();
    syslog(LOG_ERR, $_SERVER['REQUEST_URI'] . ": {$e->getMessage()}");
    new RedirectHelper('/error?id=' . $e->getErrorFile());
} catch (ViewException $e) {
    syslog(
        LOG_ERR,
        $_SERVER['REQUEST_URI'] . ": {$e->getMessage()}\n"
        . '(' . $e->getFile() . ':' . $e->getLine() . ")\n"
        . $e->getTraceAsString()
        . "\tPOST: " . json_encode($_POST) . "\n"
        . "\tSESSION: " . json_encode($_SESSION) . "\n"
    );
    ob_clean();
    new RedirectHelper('/error?id=' . $e->getErrorFile());
} catch (Exception $e) {
    syslog(
        LOG_WARN,
        $_SERVER['REQUEST_URI'] . ": {$e->getMessage()}\n"
        . '(' . $e->getFile() . ':' . $e->getLine() . ")\n"
        . $e->getTraceAsString()
        . "\tPOST: " . json_encode($_POST) . "\n"
        . "\tSESSION: " . json_encode($_SESSION) . "\n"
    );
    ob_clean();
    new RedirectHelper('/error?id=' . (new ViewException(''))->getErrorFile());
}

function makeRouter($request)
{
    $router = new \Olymp\Router(function ($method, $path, $code, $ex) use ($request) {
        if ($ex->getMessage() === 'No route found') {
            $d = new Dispatcher();
            $d->dispatch($request);
        } else {
            throw $ex;
        }
    }, 'Olymp.Controller');
    // $router->get('/articles/([0-9]+)/comments/delete/([0-9]+)', 'Controller_News::delete');
    $router->get('/', '@Home::get');
    $router->get('/home', '@Home::get');
    $router->get('/error', '@Error::get');
    $router->get('/video', '@Video::get');
    $router->get('/kontakt', '@Kontakt::get');
    $router->get('/aktualne', '@Aktualne::list');
    $router->get('/aktualne/([0-9]+)', '@Aktualne::single');
    $router->get('/oklubu/klubovi-treneri', '@Oklubu::klubovi');
    $router->get('/oklubu/externi-treneri', '@Oklubu::externi');
    $router->get('/oklubu/saly', '@Oklubu::saly');
    $router->get('/nopassword', '@Nopassword::get');
    $router->post('/nopassword', '@Nopassword::post');
    $router->get('/registrace', '@Registrace::get');
    $router->post('/registrace', '@Registrace::post');
    $router->get('/member', '@Member::get');
    $router->get('/member/home', '@Member::get');
    $router->get('/member/profil/par', '@Member.ProfilPar::get');
    $router->get('/member/profil/par/body', '@Member.ProfilPar::bodyGet');
    $router->post('/member/profil/par/body', '@Member.ProfilPar::bodyPost');
    $router->get('/member/profil/par/partner', '@Member.ProfilPar::partnerPost');
    $router->post('/member/profil/par/partner', '@Member.ProfilPar::partnerPost');
    $router->post('/member/profil/par/zadost', '@Member.ProfilPar::zadost');
    $router->get('/admin/konzole', '@Admin.Repl::get');
    $router->post('/admin/konzole', '@Admin.Repl::post');

    return $router;
}
