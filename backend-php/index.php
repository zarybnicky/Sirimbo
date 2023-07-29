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
        $user = \Session::loadUser($_SESSION['id']);
        if (!$user->getGdprSignedAt() || $user->getGdprSignedAt() === '0000-00-00 00:00:00') {
            if (!in_array(explode('?', $_SERVER['REQUEST_URI'])[0], ['/member/profil/gdpr', '/logout'])) {
                \Redirect::to('/member/profil/gdpr');
            }
        } elseif (!$user->isValid()) {
            if (!in_array(explode('?', $_SERVER['REQUEST_URI'])[0], ['/member/profil/edit', '/logout'])) {
                \Message::warning('Prosím vyplňte požadované údaje.');
                \Redirect::to('/member/profil/edit');
            }
        }
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
    $router->get('/member/akce/([0-9]+)', fn($id) => header("Location: /akce/$id"));

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
    $router->get('/admin/pary', \Olymp\Controller\Page::paryList(...));
    $router->get('/admin/pary/([0-9]+)', \Olymp\Controller\Page::parySingle(...));
    $router->get('/aktualne', \Olymp\Controller\Page::articles(...));
    $router->get('/admin/users', \Olymp\Controller\Page::userList(...));

    $router->get('/clanky/([0-9]+)', \Olymp\Controller\Aktualne::single(...));
    $router->get('/clanky/([0-9]+)/([^/]+)', \Olymp\Controller\Aktualne::single(...));

    $router->get('/login', \Olymp\Controller\Member::login(...));
    $router->post('/login', \Olymp\Controller\Member::loginPost(...));
    $router->get('/logout', \Olymp\Controller\Member::logout(...));
    $router->get('/nopassword', \Olymp\Controller\Nopassword::get(...));
    $router->post('/nopassword', \Olymp\Controller\Nopassword::post(...));
    $router->get('/registrace', \Olymp\Controller\Registrace::get(...));
    $router->post('/registrace', \Olymp\Controller\Registrace::post(...));

    $router->get('/member/dokumenty', \Olymp\Controller\Member::dokumenty(...));
    $router->get('/member/clenove', \Olymp\Controller\Member\Clenove::structure(...));
    $router->get('/member/clenove/seznam', \Olymp\Controller\Member\Clenove::list(...));
    $router->get('/member/clenove/([0-9]+)', \Olymp\Controller\Member\Clenove::single(...));

    $router->get('/member/profil', \Olymp\Controller\Member\Profil::get(...));
    $router->get('/member/profil/edit', \Olymp\Controller\Member\Profil::edit(...));
    $router->post('/member/profil/edit', \Olymp\Controller\Member\Profil::editPost(...));
    $router->get('/member/profil/gdpr', \Olymp\Controller\Member\Profil::gdpr(...));
    $router->post('/member/profil/gdpr', \Olymp\Controller\Member\Profil::gdprPost(...));
    $router->get('/member/profil/heslo', \Olymp\Controller\Member\Profil::heslo(...));
    $router->post('/member/profil/heslo', \Olymp\Controller\Member\Profil::hesloPost(...));

    $router->get('/admin/akce', \Olymp\Controller\Admin\Akce::list(...));
    $router->get('/admin/akce/add', \Olymp\Controller\Admin\Akce::add(...));
    $router->post('/admin/akce/add', \Olymp\Controller\Admin\Akce::addPost(...));
    $router->get('/admin/akce/edit/([0-9]+)', \Olymp\Controller\Admin\Akce::edit(...));
    $router->post('/admin/akce/edit/([0-9]+)', \Olymp\Controller\Admin\Akce::editPost(...));
    $router->get('/admin/akce/remove/([0-9]+)', \Olymp\Controller\Admin\Akce::remove(...));
    $router->post('/admin/akce/remove/([0-9]+)', \Olymp\Controller\Admin\Akce::removePost(...));
    $router->get('/admin/akce/detail/([0-9]+)', \Olymp\Controller\Admin\Akce::detail(...));
    $router->post('/admin/akce/detail/([0-9]+)', \Olymp\Controller\Admin\Akce::detailPost(...));

    $router->get('/admin/aktuality', \Olymp\Controller\Admin\Aktuality::list(...));
    $router->get('/admin/aktuality/add', \Olymp\Controller\Admin\Aktuality::add(...));
    $router->post('/admin/aktuality/add', \Olymp\Controller\Admin\Aktuality::addPost(...));
    $router->get('/admin/aktuality/edit/([0-9]+)', \Olymp\Controller\Admin\Aktuality::edit(...));
    $router->post('/admin/aktuality/edit/([0-9]+)', \Olymp\Controller\Admin\Aktuality::editPost(...));
    $router->get('/admin/aktuality/remove/([0-9]+)', \Olymp\Controller\Admin\Aktuality::remove(...));
    $router->post('/admin/aktuality/remove/([0-9]+)', \Olymp\Controller\Admin\Aktuality::removePost(...));
    $router->get('/admin/aktuality/foto/([0-9]+)', \Olymp\Controller\Admin\Aktuality::foto(...));
    $router->post('/admin/aktuality/foto/([0-9]+)', \Olymp\Controller\Admin\Aktuality::fotoPost(...));

    $router->get('/admin/nastenka', \Olymp\Controller\Admin\Nastenka::list(...));
    $router->get('/admin/nastenka/add', \Olymp\Controller\Admin\Nastenka::add(...));
    $router->post('/admin/nastenka/add', \Olymp\Controller\Admin\Nastenka::addPost(...));
    $router->get('/admin/nastenka/([0-9]+)', \Olymp\Controller\Admin\Nastenka::edit(...));
    $router->post('/admin/nastenka/([0-9]+)', \Olymp\Controller\Admin\Nastenka::editPost(...));
    $router->get('/admin/nastenka/remove/([0-9]+)', \Olymp\Controller\Admin\Nastenka::remove(...));
    $router->post('/admin/nastenka/remove/([0-9]+)', \Olymp\Controller\Admin\Nastenka::removePost(...));

    $router->get('/admin/users/add', \Olymp\Controller\Admin\Users::add(...));
    $router->post('/admin/users/add', \Olymp\Controller\Admin\Users::addPost(...));
    $router->get('/admin/users/([0-9]+)', \Olymp\Controller\Admin\Users::edit(...));
    $router->post('/admin/users/([0-9]+)', \Olymp\Controller\Admin\Users::editPost(...));
    $router->get('/admin/users/remove/([0-9]+)', \Olymp\Controller\Admin\Users::remove(...));
    $router->post('/admin/users/remove/([0-9]+)', \Olymp\Controller\Admin\Users::removePost(...));
    $router->get('/admin/users/unconfirmed', \Olymp\Controller\Admin\Users::unconfirmed(...));
    $router->post('/admin/users/unconfirmed', \Olymp\Controller\Admin\Users::unconfirmedPost(...));

    $router->get('/admin/galerie', \Olymp\Controller\Admin\Galerie::list(...));
    $router->get('/admin/galerie/file/upload', \Olymp\Controller\Admin\GalerieFile::upload(...));
    $router->post('/admin/galerie/file/upload', \Olymp\Controller\Admin\GalerieFile::uploadPost(...));
    $router->get('/admin/galerie/file/remove/([0-9]+)', \Olymp\Controller\Admin\GalerieFile::remove(...));
    $router->post('/admin/galerie/file/remove/([0-9]+)', \Olymp\Controller\Admin\GalerieFile::removePost(...));
    $router->get('/admin/galerie/directory/add', \Olymp\Controller\Admin\GalerieDirectory::add(...));
    $router->post('/admin/galerie/directory/add', \Olymp\Controller\Admin\GalerieDirectory::addPost(...));
    $router->get('/admin/galerie/directory/([0-9]+)', \Olymp\Controller\Admin\GalerieDirectory::list(...));
    $router->get('/admin/galerie/directory/edit/([0-9]+)', \Olymp\Controller\Admin\GalerieDirectory::edit(...));
    $router->post('/admin/galerie/directory/edit/([0-9]+)', \Olymp\Controller\Admin\GalerieDirectory::editPost(...));
    $router->get('/admin/galerie/directory/remove/([0-9]+)', \Olymp\Controller\Admin\GalerieDirectory::remove(...));
    $router->post('/admin/galerie/directory/remove/([0-9]+)', \Olymp\Controller\Admin\GalerieDirectory::removePost(...));

    $router->get('/admin/rozpis', \Olymp\Controller\Admin\Rozpis::list(...));
    $router->get('/admin/rozpis/add', \Olymp\Controller\Admin\Rozpis::add(...));
    $router->post('/admin/rozpis/add', \Olymp\Controller\Admin\Rozpis::addPost(...));
    $router->get('/admin/rozpis/duplicate/([0-9]+)', \Olymp\Controller\Admin\Rozpis::duplicate(...));
    $router->get('/admin/rozpis/edit/([0-9]+)', \Olymp\Controller\Admin\Rozpis::edit(...));
    $router->post('/admin/rozpis/edit/([0-9]+)', \Olymp\Controller\Admin\Rozpis::editPost(...));
    $router->get('/admin/rozpis/remove/([0-9]+)', \Olymp\Controller\Admin\Rozpis::remove(...));
    $router->get('/admin/rozpis/detail/([0-9]+)', \Olymp\Controller\Admin\RozpisDetail::detail(...));
    $router->post('/admin/rozpis/detail/([0-9]+)', \Olymp\Controller\Admin\RozpisDetail::detailPost(...));

    $router->get('/admin/nabidka', \Olymp\Controller\Admin\Nabidka::list(...));
    $router->get('/admin/nabidka/add', \Olymp\Controller\Admin\Nabidka::add(...));
    $router->post('/admin/nabidka/add', \Olymp\Controller\Admin\Nabidka::addPost(...));
    $router->get('/admin/nabidka/duplicate/([0-9]+)', \Olymp\Controller\Admin\Nabidka::duplicate(...));
    $router->get('/admin/nabidka/edit/([0-9]+)', \Olymp\Controller\Admin\Nabidka::edit(...));
    $router->post('/admin/nabidka/edit/([0-9]+)', \Olymp\Controller\Admin\Nabidka::editPost(...));
    $router->get('/admin/nabidka/remove/([0-9]+)', \Olymp\Controller\Admin\Nabidka::remove(...));
    $router->get('/admin/nabidka/detail/([0-9]+)', \Olymp\Controller\Admin\NabidkaDetail::detail(...));
    $router->post('/admin/nabidka/detail/([0-9]+)', \Olymp\Controller\Admin\NabidkaDetail::detailPost(...));

    $router->get('/admin/dokumenty', \Olymp\Controller\Admin\Dokumenty::list(...));
    $router->post('/admin/dokumenty', \Olymp\Controller\Admin\Dokumenty::listPost(...));
    $router->get('/admin/dokumenty/remove/([0-9]+)', \Olymp\Controller\Admin\Dokumenty::remove(...));
    $router->post('/admin/dokumenty/remove/([0-9]+)', \Olymp\Controller\Admin\Dokumenty::removePost(...));

    $router->get('/admin/skupiny', \Olymp\Controller\Admin\Skupiny::list(...));
    $router->get('/admin/skupiny/add', \Olymp\Controller\Admin\Skupiny::add(...));
    $router->post('/admin/skupiny/add', \Olymp\Controller\Admin\Skupiny::addPost(...));
    $router->get('/admin/skupiny/edit/([0-9]+)', \Olymp\Controller\Admin\Skupiny::edit(...));
    $router->post('/admin/skupiny/edit/([0-9]+)', \Olymp\Controller\Admin\Skupiny::editPost(...));
    $router->get('/admin/skupiny/remove/([0-9]+)', \Olymp\Controller\Admin\Skupiny::remove(...));
    $router->post('/admin/skupiny/remove/([0-9]+)', \Olymp\Controller\Admin\Skupiny::removePost(...));

    $router->get('/admin/platby', \Olymp\Controller\Admin\Platby::overview(...));
    $router->get('/admin/platby/structure', \Olymp\Controller\Admin\Platby::structure(...));
    $router->get('/admin/platby/raw', \Olymp\Controller\Admin\PlatbyRaw::get(...));
    $router->post('/admin/platby/raw', \Olymp\Controller\Admin\PlatbyRaw::post(...));
    $router->get('/admin/platby/raw/select_columns', \Olymp\Controller\Admin\PlatbyRaw::selectColumns(...));
    $router->post('/admin/platby/raw/select_columns', \Olymp\Controller\Admin\PlatbyRaw::selectColumnsPost(...));
    $router->get('/admin/platby/discarded', \Olymp\Controller\Admin\PlatbyDiscarded::view(...));
    $router->get('/admin/platby/discarded/remove/([0-9]+)', \Olymp\Controller\Admin\PlatbyDiscarded::remove(...));
    $router->get('/admin/platby/manual', \Olymp\Controller\Admin\PlatbyManual::query(...));
    $router->get('/admin/platby/manual/([0-9]+)', \Olymp\Controller\Admin\PlatbyManual::get(...));
    $router->post('/admin/platby/manual/([0-9]+)', \Olymp\Controller\Admin\PlatbyManual::post(...));

    $router->get('/admin/platby/items', \Olymp\Controller\Admin\PlatbyItems::list(...));
    $router->get('/admin/platby/items/add', \Olymp\Controller\Admin\PlatbyItems::add(...));
    $router->post('/admin/platby/items/add', \Olymp\Controller\Admin\PlatbyItems::addPost(...));
    $router->get('/admin/platby/items/edit/([0-9]+)', \Olymp\Controller\Admin\PlatbyItems::edit(...));
    $router->post('/admin/platby/items/edit/([0-9]+)', \Olymp\Controller\Admin\PlatbyItems::editPost(...));
    $router->get('/admin/platby/items/remove/([0-9]+)', \Olymp\Controller\Admin\PlatbyItems::remove(...));
    $router->post('/admin/platby/items/remove/([0-9]+)', \Olymp\Controller\Admin\PlatbyItems::removePost(...));

    $router->get('/admin/platby/structure/group', \Olymp\Controller\Admin\PlatbyGroup::list(...));
    $router->get('/admin/platby/structure/group/add', \Olymp\Controller\Admin\PlatbyGroup::add(...));
    $router->post('/admin/platby/structure/group/add', \Olymp\Controller\Admin\PlatbyGroup::addPost(...));
    $router->get('/admin/platby/structure/group/edit/([0-9]+)', \Olymp\Controller\Admin\PlatbyGroup::edit(...));
    $router->post('/admin/platby/structure/group/edit/([0-9]+)', \Olymp\Controller\Admin\PlatbyGroup::editPost(...));
    $router->get('/admin/platby/structure/group/remove/([0-9]+)', \Olymp\Controller\Admin\PlatbyGroup::remove(...));
    $router->post('/admin/platby/structure/group/remove/([0-9]+)', \Olymp\Controller\Admin\PlatbyGroup::removePost(...));

    $router->get('/admin/platby/structure/category', \Olymp\Controller\Admin\PlatbyCategory::list(...));
    $router->post('/admin/platby/structure/category', \Olymp\Controller\Admin\PlatbyCategory::listPost(...));
    $router->get('/admin/platby/structure/category/add', \Olymp\Controller\Admin\PlatbyCategory::add(...));
    $router->post('/admin/platby/structure/category/add', \Olymp\Controller\Admin\PlatbyCategory::addPost(...));
    $router->get('/admin/platby/structure/category/edit/([0-9]+)', \Olymp\Controller\Admin\PlatbyCategory::edit(...));
    $router->post('/admin/platby/structure/category/edit/([0-9]+)', \Olymp\Controller\Admin\PlatbyCategory::editPost(...));
    $router->get('/admin/platby/structure/category/remove/([0-9]+)', \Olymp\Controller\Admin\PlatbyCategory::remove(...));
    $router->post('/admin/platby/structure/category/remove/([0-9]+)', \Olymp\Controller\Admin\PlatbyCategory::removePost(...));

    return $router;
}
