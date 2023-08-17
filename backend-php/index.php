<?php
require 'vendor/autoload.php';
require 'config.php';

Sentry\init([
    'environment' => SENTRY_ENV,
    'dsn' => 'https://943ee3e7e7044524b2ee8413a957e14f@o775093.ingest.sentry.io/5796825',
    'traces_sample_rate' => 1,
]);

require 'files/Core/settings.php';

ob_start();
date_default_timezone_set('Europe/Paris');
mb_internal_encoding('UTF-8');
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
    if ($_SESSION['id'] ?? null) {
        \Session::loadUser($_SESSION['id']);
    } elseif (($_POST['action'] ?? '') == 'login') {
        if (!\Session::login($_POST['login'], User::crypt($_POST['pass']))) {
            \Message::danger('Špatné jméno nebo heslo!');
            \Redirect::to('/login');
        } elseif ($_GET['return'] ?? null) {
            \Redirect::to($_GET['return']);
        } else {
            \Redirect::to('/member');
        }
    }

    $router = makeRouter();
    $router->dispatchGlobal();
} catch (ViewException $e) {
    if ($e->getErrorFile() === 'authorization') {
        ob_clean();
        http_response_code(403);
        \Render::twig('Error.twig', ['errorCode' => 'authorization']);
    } elseif ($e->getErrorFile() === 'not_found') {
        ob_clean();
        syslog(LOG_ERR, "{$_SERVER['REQUEST_URI']}: {$e->getMessage()}");
        http_response_code(404);
        \Render::twig('Error.twig', ['errorCode' => 'not_found']);
    } else {
        \Sentry\captureException($e);
        syslog(
            LOG_ERR,
            $_SERVER['REQUEST_URI'] . ": {$e->getMessage()}\n"
            . '(' . $e->getFile() . ':' . $e->getLine() . ")\n"
            . $e->getTraceAsString()
            . "\tPOST: " . json_encode($_POST) . "\n"
            . "\tSESSION: " . json_encode($_SESSION) . "\n"
        );
        ob_clean();
        \Render::twig('Error.twig', ['errorCode' => $e->getErrorFile()]);
    }
} catch (Exception $e) {
    \Sentry\captureException($e);
    syslog(
        LOG_ERR,
        $_SERVER['REQUEST_URI'] . ": {$e->getMessage()}\n"
        . '(' . $e->getFile() . ':' . $e->getLine() . ")\n"
        . $e->getTraceAsString()
        . "\tPOST: " . json_encode($_POST) . "\n"
        . "\tSESSION: " . json_encode($_SESSION) . "\n"
    );
    ob_clean();
    \Render::twig('Error.twig', ['errorCode' => 'script_fatal']);
}
\Sentry\captureLastError();

function makeRouter(): \Olymp\Router
{
    $router = new \Olymp\Router();

    $router->redirect('/home', '/');
    $router->redirect('/member/home', '/member');
    $router->redirect('/member/rozpis', '/schedule');
    $router->redirect('/member/nabidka', '/schedule');
    $router->redirect('/member/treninky', '/schedule');
    $router->redirect('/member/clenove/structure', '/member/clenove');
    $router->redirect('/member/akce', '/akce');
    $router->redirect('/aktualne', '/clanky');
    $router->get('/member/akce/([0-9]+)', fn($id) => header("Location: /akce/$id"));
    $router->get('/users/([0-9]+)', fn($id) => header("Location: https://olymp.zarybnicky.com/users/$id"));

    $router->get('/', \Olymp\Controller\Home::get(...));

    $router->get('/error', \Olymp\Controller\Page::error(...));
    $router->redirect('/prijdtancit', 'https://olymp.zarybnicky.com/prijdtancit');
    $router->get('/ochrana-osobnich-udaju', \Olymp\Controller\Page::ochranaUdaju(...));
    $router->get('/kontakt', \Olymp\Controller\Page::kontakt(...));
    $router->get('/oklubu/klubovi-treneri', \Olymp\Controller\Page::klubovi(...));
    $router->get('/oklubu/externi-treneri', \Olymp\Controller\Page::externi(...));
    $router->get('/oklubu/saly', \Olymp\Controller\Page::saly(...));
    $router->get('/schedule', \Olymp\Controller\Page::schedule(...));
    $router->get('/akce', \Olymp\Controller\Page::akce(...));
    $router->get('/akce/([0-9]+)', \Olymp\Controller\Page::akceSingle(...));
    $router->get('/member', \Olymp\Controller\Page::nastenka(...));
    $router->get('/pary', \Olymp\Controller\Page::paryList(...));
    $router->get('/pary/([0-9]+)', \Olymp\Controller\Page::parySingle(...));
    $router->get('/users', \Olymp\Controller\Page::userList(...));

    $router->get('/clanky', \Olymp\Controller\Page::articles(...));
    $router->get('/clanky/([0-9]+)', \Olymp\Controller\Aktualne::single(...));
    $router->get('/clanky/([0-9]+)/([^/]+)', \Olymp\Controller\Aktualne::single(...));

    $router->get('/login', \Olymp\Controller\Member::login(...));
    $router->post('/login', \Olymp\Controller\Member::loginPost(...));
    $router->get('/logout', \Olymp\Controller\Member::logout(...));
    $router->get('/nopassword', \Olymp\Controller\Nopassword::get(...));
    $router->post('/nopassword', \Olymp\Controller\Nopassword::post(...));

    $router->get('/member/dokumenty', \Olymp\Controller\Member::dokumenty(...));
    $router->get('/member/clenove', \Olymp\Controller\Member\Clenove::structure(...));
    $router->get('/member/clenove/seznam', \Olymp\Controller\Member\Clenove::list(...));
    $router->get('/member/clenove/([0-9]+)', \Olymp\Controller\Member\Clenove::single(...));

    $router->get('/member/profil', \Olymp\Controller\Member\Profil::get(...));
    $router->get('/member/profil/heslo', \Olymp\Controller\Member\Profil::heslo(...));
    $router->post('/member/profil/heslo', \Olymp\Controller\Member\Profil::hesloPost(...));

    // $router->get('/akce', \Olymp\Controller\Admin\Akce::list(...));
    // $router->get('/akce/add', \Olymp\Controller\Admin\Akce::add(...));
    // $router->post('/akce/add', \Olymp\Controller\Admin\Akce::addPost(...));
    // $router->get('/akce/edit/([0-9]+)', \Olymp\Controller\Admin\Akce::edit(...));
    // $router->post('/akce/edit/([0-9]+)', \Olymp\Controller\Admin\Akce::editPost(...));
    // $router->get('/akce/remove/([0-9]+)', \Olymp\Controller\Admin\Akce::remove(...));
    // $router->post('/akce/remove/([0-9]+)', \Olymp\Controller\Admin\Akce::removePost(...));
    // $router->get('/akce/detail/([0-9]+)', \Olymp\Controller\Admin\Akce::detail(...));
    // $router->post('/akce/detail/([0-9]+)', \Olymp\Controller\Admin\Akce::detailPost(...));

    $router->get('/aktuality', \Olymp\Controller\Admin\Aktuality::list(...));
    $router->get('/aktuality/add', \Olymp\Controller\Admin\Aktuality::add(...));
    $router->post('/aktuality/add', \Olymp\Controller\Admin\Aktuality::addPost(...));
    $router->get('/aktuality/edit/([0-9]+)', \Olymp\Controller\Admin\Aktuality::edit(...));
    $router->post('/aktuality/edit/([0-9]+)', \Olymp\Controller\Admin\Aktuality::editPost(...));
    $router->get('/aktuality/remove/([0-9]+)', \Olymp\Controller\Admin\Aktuality::remove(...));
    $router->post('/aktuality/remove/([0-9]+)', \Olymp\Controller\Admin\Aktuality::removePost(...));
    $router->get('/aktuality/foto/([0-9]+)', \Olymp\Controller\Admin\Aktuality::foto(...));
    $router->post('/aktuality/foto/([0-9]+)', \Olymp\Controller\Admin\Aktuality::fotoPost(...));

    $router->get('/nastenka', \Olymp\Controller\Admin\Nastenka::list(...));
    $router->get('/nastenka/add', \Olymp\Controller\Admin\Nastenka::add(...));
    $router->post('/nastenka/add', \Olymp\Controller\Admin\Nastenka::addPost(...));
    $router->get('/nastenka/([0-9]+)', \Olymp\Controller\Admin\Nastenka::edit(...));
    $router->post('/nastenka/([0-9]+)', \Olymp\Controller\Admin\Nastenka::editPost(...));
    $router->get('/nastenka/remove/([0-9]+)', \Olymp\Controller\Admin\Nastenka::remove(...));
    $router->post('/nastenka/remove/([0-9]+)', \Olymp\Controller\Admin\Nastenka::removePost(...));

    $router->get('/users/([0-9]+)', \Olymp\Controller\Admin\Users::edit(...));

    $router->get('/galerie', \Olymp\Controller\Admin\Galerie::list(...));
    $router->get('/galerie/file/upload', \Olymp\Controller\Admin\GalerieFile::upload(...));
    $router->post('/galerie/file/upload', \Olymp\Controller\Admin\GalerieFile::uploadPost(...));
    $router->get('/galerie/file/remove/([0-9]+)', \Olymp\Controller\Admin\GalerieFile::remove(...));
    $router->post('/galerie/file/remove/([0-9]+)', \Olymp\Controller\Admin\GalerieFile::removePost(...));
    $router->get('/galerie/directory/add', \Olymp\Controller\Admin\GalerieDirectory::add(...));
    $router->post('/galerie/directory/add', \Olymp\Controller\Admin\GalerieDirectory::addPost(...));
    $router->get('/galerie/directory/([0-9]+)', \Olymp\Controller\Admin\GalerieDirectory::list(...));
    $router->get('/galerie/directory/edit/([0-9]+)', \Olymp\Controller\Admin\GalerieDirectory::edit(...));
    $router->post('/galerie/directory/edit/([0-9]+)', \Olymp\Controller\Admin\GalerieDirectory::editPost(...));
    $router->get('/galerie/directory/remove/([0-9]+)', \Olymp\Controller\Admin\GalerieDirectory::remove(...));
    $router->post('/galerie/directory/remove/([0-9]+)', \Olymp\Controller\Admin\GalerieDirectory::removePost(...));

    $router->get('/dokumenty', \Olymp\Controller\Admin\Dokumenty::list(...));
    $router->post('/dokumenty', \Olymp\Controller\Admin\Dokumenty::listPost(...));
    $router->get('/dokumenty/remove/([0-9]+)', \Olymp\Controller\Admin\Dokumenty::remove(...));
    $router->post('/dokumenty/remove/([0-9]+)', \Olymp\Controller\Admin\Dokumenty::removePost(...));

    $router->get('/skupiny', \Olymp\Controller\Admin\Skupiny::list(...));
    $router->get('/skupiny/add', \Olymp\Controller\Admin\Skupiny::add(...));
    $router->post('/skupiny/add', \Olymp\Controller\Admin\Skupiny::addPost(...));
    $router->get('/skupiny/edit/([0-9]+)', \Olymp\Controller\Admin\Skupiny::edit(...));
    $router->post('/skupiny/edit/([0-9]+)', \Olymp\Controller\Admin\Skupiny::editPost(...));
    $router->get('/skupiny/remove/([0-9]+)', \Olymp\Controller\Admin\Skupiny::remove(...));
    $router->post('/skupiny/remove/([0-9]+)', \Olymp\Controller\Admin\Skupiny::removePost(...));

    $router->get('/platby', \Olymp\Controller\Admin\Platby::overview(...));
    $router->get('/platby/structure', \Olymp\Controller\Admin\Platby::structure(...));
    $router->get('/platby/raw', \Olymp\Controller\Admin\PlatbyRaw::get(...));
    $router->post('/platby/raw', \Olymp\Controller\Admin\PlatbyRaw::post(...));
    $router->get('/platby/raw/select_columns', \Olymp\Controller\Admin\PlatbyRaw::selectColumns(...));
    $router->post('/platby/raw/select_columns', \Olymp\Controller\Admin\PlatbyRaw::selectColumnsPost(...));
    $router->get('/platby/discarded', \Olymp\Controller\Admin\PlatbyDiscarded::view(...));
    $router->get('/platby/discarded/remove/([0-9]+)', \Olymp\Controller\Admin\PlatbyDiscarded::remove(...));
    $router->get('/platby/manual', \Olymp\Controller\Admin\PlatbyManual::query(...));
    $router->get('/platby/manual/([0-9]+)', \Olymp\Controller\Admin\PlatbyManual::get(...));
    $router->post('/platby/manual/([0-9]+)', \Olymp\Controller\Admin\PlatbyManual::post(...));

    $router->get('/platby/items', \Olymp\Controller\Admin\PlatbyItems::list(...));
    $router->get('/platby/items/add', \Olymp\Controller\Admin\PlatbyItems::add(...));
    $router->post('/platby/items/add', \Olymp\Controller\Admin\PlatbyItems::addPost(...));
    $router->get('/platby/items/edit/([0-9]+)', \Olymp\Controller\Admin\PlatbyItems::edit(...));
    $router->post('/platby/items/edit/([0-9]+)', \Olymp\Controller\Admin\PlatbyItems::editPost(...));
    $router->get('/platby/items/remove/([0-9]+)', \Olymp\Controller\Admin\PlatbyItems::remove(...));
    $router->post('/platby/items/remove/([0-9]+)', \Olymp\Controller\Admin\PlatbyItems::removePost(...));

    $router->get('/platby/structure/group', \Olymp\Controller\Admin\PlatbyGroup::list(...));
    $router->get('/platby/structure/group/add', \Olymp\Controller\Admin\PlatbyGroup::add(...));
    $router->post('/platby/structure/group/add', \Olymp\Controller\Admin\PlatbyGroup::addPost(...));
    $router->get('/platby/structure/group/edit/([0-9]+)', \Olymp\Controller\Admin\PlatbyGroup::edit(...));
    $router->post('/platby/structure/group/edit/([0-9]+)', \Olymp\Controller\Admin\PlatbyGroup::editPost(...));
    $router->get('/platby/structure/group/remove/([0-9]+)', \Olymp\Controller\Admin\PlatbyGroup::remove(...));
    $router->post('/platby/structure/group/remove/([0-9]+)', \Olymp\Controller\Admin\PlatbyGroup::removePost(...));

    $router->get('/platby/structure/category', \Olymp\Controller\Admin\PlatbyCategory::list(...));
    $router->post('/platby/structure/category', \Olymp\Controller\Admin\PlatbyCategory::listPost(...));
    $router->get('/platby/structure/category/add', \Olymp\Controller\Admin\PlatbyCategory::add(...));
    $router->post('/platby/structure/category/add', \Olymp\Controller\Admin\PlatbyCategory::addPost(...));
    $router->get('/platby/structure/category/edit/([0-9]+)', \Olymp\Controller\Admin\PlatbyCategory::edit(...));
    $router->post('/platby/structure/category/edit/([0-9]+)', \Olymp\Controller\Admin\PlatbyCategory::editPost(...));
    $router->get('/platby/structure/category/remove/([0-9]+)', \Olymp\Controller\Admin\PlatbyCategory::remove(...));
    $router->post('/platby/structure/category/remove/([0-9]+)', \Olymp\Controller\Admin\PlatbyCategory::removePost(...));

    return $router;
}
