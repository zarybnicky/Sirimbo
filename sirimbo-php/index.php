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

if (!isset($_SERVER['HTTP_REFERER'])) {
    $_SERVER['HTTP_REFERER'] = getallheaders()['Referer'] ?? null;
}

session_set_save_handler(new DbSessionHandler(), true);
session_start();

try {
    if (isset($_SESSION['login']) && $_SESSION['login'] !== null) {
        \Session::loadUser($_SESSION['id']);
        if ($_SESSION['gdpr']) {
            if (!in_array(explode('?', $_SERVER['REQUEST_URI'])[0], ['/member/profil/gdpr', '/logout'])) {
                return new RedirectHelper('/member/profil/gdpr');
            }
        } elseif ($_SESSION['invalid']) {
            if (!in_array(explode('?', $_SERVER['REQUEST_URI'])[0], ['/member/profil/edit', '/logout'])) {
                return new RedirectHelper('/member/profil/edit', 'Prosím vyplňte požadované údaje.');
            }
        }
    } elseif (isset($_POST['login']) && isset($_POST['pass'])) {
        $_POST['pass'] = User::crypt($_POST['pass']);

        if (!\Session::login($_POST['login'], $_POST['pass'])) {
            new RedirectHelper('/login', 'Špatné jméno nebo heslo!');
        } elseif ($_GET['return'] ?? null) {
            new RedirectHelper($_GET['return']);
        } else {
            new RedirectHelper('/member');
        }
    }

    $router = makeRouter();
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

function makeRouter()
{
    $router = new \Olymp\Router(function ($method, $path, $code, $ex) {
        if ($ex->getMessage() === 'No route found') {
            $request = new Request($_SERVER['REQUEST_URI'], $_SERVER['REQUEST_METHOD']);
            $d = new Dispatcher();
            $d->dispatch($request);
        } else {
            throw $ex;
        }
    }, 'Olymp.Controller');

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
    $router->get('/fotogalerie', '@Fotogalerie::root');
    $router->get('/fotogalerie/([0-9]+)', '@Fotogalerie::directory');
    $router->get('/fotogalerie/foto/([0-9]+)', '@Fotogalerie::single');

    $router->get('/login', '@Member::login');
    $router->get('/nopassword', '@Nopassword::get');
    $router->post('/nopassword', '@Nopassword::post');
    $router->get('/registrace', '@Registrace::get');
    $router->post('/registrace', '@Registrace::post');

    $router->get('/member', '@Member::get');
    $router->get('/member/home', '@Member::get');
    $router->get('/member/akce', '@Member.Akce::list');
    $router->post('/member/akce', '@Member.Akce::listPost');
    $router->get('/member/akce/([0-9]+)', '@Member.Akce::single');
    $router->get('/member/nabidka', '@Member.Nabidka::get');
    $router->post('/member/nabidka', '@Member.Nabidka::post');
    $router->get('/member/rozpis', '@Member.Rozpis::get');
    $router->post('/member/rozpis', '@Member.Rozpis::post');
    $router->get('/member/dokumenty', '@Member::dokumenty');
    $router->get('/member/download', '@Member::download');
    $router->get('/member/clenove', '@Member.Clenove::structure');
    $router->get('/member/clenove/structure', '@Member.Clenove::structure');
    $router->get('/member/clenove/seznam', '@Member.Clenove::list');
    $router->get('/member/clenove/skupiny', '@Member.Clenove::groups');
    $router->get('/member/clenove/([0-9]+)', '@Member.Clenove::single');

    $router->get('/member/profil', '@Member.Profil::get');
    $router->get('/member/profil/edit', '@Member.Profil::edit');
    $router->post('/member/profil/edit', '@Member.Profil::editPost');
    $router->get('/member/profil/gdpr', '@Member.Profil::gdpr');
    $router->post('/member/profil/gdpr', '@Member.Profil::gdprPost');
    $router->get('/member/profil/heslo', '@Member.Profil::heslo');
    $router->post('/member/profil/heslo', '@Member.Profil::hesloPost');
    $router->get('/member/profil/par/body', '@Member.ProfilPar::body');
    $router->post('/member/profil/par/body', '@Member.ProfilPar::bodyPost');
    $router->get('/member/profil/par/partner', '@Member.ProfilPar::partner');
    $router->post('/member/profil/par/partner', '@Member.ProfilPar::partnerPost');
    $router->post('/member/profil/par/zadost', '@Member.ProfilPar::zadost');
    $router->get('/member/profil/pary', '@Member.ProfilPar::pary');

    $router->get('/admin/konzole', '@Admin.Repl::get');
    $router->post('/admin/konzole', '@Admin.Repl::post');
    $router->get('/admin/akce', '@Admin.Akce::list');
    $router->post('/admin/akce', '@Admin.Akce::listPost');
    $router->get('/admin/akce/add', '@Admin.Akce::add');
    $router->post('/admin/akce/add', '@Admin.Akce::addPost');
    $router->get('/admin/akce/edit/([0-9]+)', '@Admin.Akce::edit');
    $router->post('/admin/akce/edit/([0-9]+)', '@Admin.Akce::editPost');
    $router->get('/admin/akce/remove/([0-9]+)', '@Admin.Akce::remove');
    $router->post('/admin/akce/remove/([0-9]+)', '@Admin.Akce::removePost');
    $router->get('/admin/akce/detail/([0-9]+)', '@Admin.Akce::detail');
    $router->post('/admin/akce/detail/([0-9]+)', '@Admin.Akce::detailPost');
    $router->get('/admin/akce/dokumenty/([0-9]+)', '@Admin.Akce::dokumenty');
    $router->post('/admin/akce/dokumenty/([0-9]+)', '@Admin.Akce::dokumentyPost');

    return $router;
}
