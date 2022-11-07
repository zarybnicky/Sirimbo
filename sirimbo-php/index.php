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

require 'config.php';
require 'vendor/autoload.php';

Sentry\init([
    'environment' => SENTRY_ENV,
    'dsn' => 'https://943ee3e7e7044524b2ee8413a957e14f@o775093.ingest.sentry.io/5796825',
    'traces_sample_rate' => 1,
]);

ob_start();
ini_set('session.use_trans_sid', '0');
ini_set('session.use_only_cookies', '1');
ini_set('session.cookie_lifetime', '0');
ini_set('session.cookie_httponly', '1');
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
            \Redirect::to('/dashboard');
        }
    }

    $router = new \Olymp\Router();

    $router->get('/admin/users/sign-as/([0-9]+)', [\Olymp\Controller\Admin\Users::class, 'signAs']);
    $router->get('/admin/users/getMsmtCsv', [\Olymp\Controller\Admin\Users::class, 'getMsmtCsv']);

    $router->get('/admin/platby', [\Olymp\Controller\Admin\Platby::class, 'overview']);
    $router->get('/admin/platby/structure', [\Olymp\Controller\Admin\Platby::class, 'structure']);
    $router->get('/admin/platby/items', [\Olymp\Controller\Admin\PlatbyItems::class, 'list']);
    $router->get('/admin/platby/manual', [\Olymp\Controller\Admin\PlatbyManual::class, 'query']);

    $router->get('/member/akce/([0-9]+)', [\Olymp\Controller\Member\Akce::class, 'single']);
    $router->get('/member/akce', [\Olymp\Controller\Member\Akce::class, 'list']);
    $router->post('/member/akce', [\Olymp\Controller\Member\Akce::class, 'listPost']);

    $router->get('/member/download', [\Olymp\Controller\Member::class, 'download']);

    $router->get('/member/profil', [\Olymp\Controller\Member\Profil::class, 'get']);

    $router->get('/member/profil/edit', [\Olymp\Controller\Member\Profil::class, 'edit']);
    $router->post('/member/profil/edit', [\Olymp\Controller\Member\Profil::class, 'editPost']);

    $router->get('/member/profil/gdpr', [\Olymp\Controller\Member\Profil::class, 'gdpr']);
    $router->post('/member/profil/gdpr', [\Olymp\Controller\Member\Profil::class, 'gdprPost']);

    $router->get('/member/profil/heslo', [\Olymp\Controller\Member\Profil::class, 'heslo']);
    $router->post('/member/profil/heslo', [\Olymp\Controller\Member\Profil::class, 'hesloPost']);

    $router->get('/member/profil/par/partner', [\Olymp\Controller\Member\ProfilPar::class, 'partner']);
    $router->post('/member/profil/par/partner', [\Olymp\Controller\Member\ProfilPar::class, 'partnerPost']);
    $router->post('/member/profil/par/zadost', [\Olymp\Controller\Member\ProfilPar::class, 'zadost']);

    $router->get('/admin/akce/detail/([0-9]+)', [\Olymp\Controller\Admin\Akce::class, 'detail']);
    $router->post('/admin/akce/detail/([0-9]+)', [\Olymp\Controller\Admin\Akce::class, 'detailPost']);

    $router->get('/admin/akce/dokumenty/([0-9]+)', [\Olymp\Controller\Admin\Akce::class, 'dokumenty']);
    $router->post('/admin/akce/dokumenty/([0-9]+)', [\Olymp\Controller\Admin\Akce::class, 'dokumentyPost']);

    $router->get('/admin/dokumenty', [\Olymp\Controller\Admin\Dokumenty::class, 'list']);
    $router->post('/admin/dokumenty', [\Olymp\Controller\Admin\Dokumenty::class, 'listPost']);

    $router->get('/admin/video', [\Olymp\Controller\Admin\Video::class, 'orphan']);
    $router->get('/admin/video/orphan', [\Olymp\Controller\Admin\Video::class, 'orphan']);

    $router->get('/admin/video/title', [\Olymp\Controller\Admin\Video::class, 'title']);
    $router->post('/admin/video/title', [\Olymp\Controller\Admin\Video::class, 'titlePost']);

    $router->get('/admin/video/playlist', [\Olymp\Controller\Admin\Video::class, 'playlistList']);
    $router->get('/admin/video/playlist/([0-9]+)', [\Olymp\Controller\Admin\Video::class, 'playlist']);

    $router->get('/admin/video/add', [\Olymp\Controller\Admin\Video::class, 'add']);
    $router->post('/admin/video/add', [\Olymp\Controller\Admin\Video::class, 'addPost']);

    $router->get('/admin/video/edit/([0-9]+)', [\Olymp\Controller\Admin\Video::class, 'edit']);
    $router->post('/admin/video/edit/([0-9]+)', [\Olymp\Controller\Admin\Video::class, 'editPost']);

    $router->get('/admin/video/remove/([0-9]+)', [\Olymp\Controller\Admin\Video::class, 'remove']);
    $router->post('/admin/video/remove/([0-9]+)', [\Olymp\Controller\Admin\Video::class, 'removePost']);

    $router->get('/admin/nastenka/add', [\Olymp\Controller\Admin\Nastenka::class, 'add']);
    $router->post('/admin/nastenka/add', [\Olymp\Controller\Admin\Nastenka::class, 'addPost']);

    $router->get('/admin/nastenka/edit/([0-9]+)', [\Olymp\Controller\Admin\Nastenka::class, 'edit']);
    $router->post('/admin/nastenka/edit/([0-9]+)', [\Olymp\Controller\Admin\Nastenka::class, 'editPost']);

    $router->get('/admin/galerie/file/upload', [\Olymp\Controller\Admin\GalerieFile::class, 'upload']);
    $router->post('/admin/galerie/file/upload', [\Olymp\Controller\Admin\GalerieFile::class, 'uploadPost']);

    $router->get('/admin/galerie/file/edit/([0-9]+)', [\Olymp\Controller\Admin\GalerieFile::class, 'edit']);
    $router->post('/admin/galerie/file/edit/([0-9]+)', [\Olymp\Controller\Admin\GalerieFile::class, 'editPost']);

    $router->get('/admin/galerie/file/remove/([0-9]+)', [\Olymp\Controller\Admin\GalerieFile::class, 'remove']);
    $router->post('/admin/galerie/file/remove/([0-9]+)', [\Olymp\Controller\Admin\GalerieFile::class, 'removePost']);

    $router->get('/admin/galerie/directory/add', [\Olymp\Controller\Admin\GalerieDirectory::class, 'add']);
    $router->post('/admin/galerie/directory/add', [\Olymp\Controller\Admin\GalerieDirectory::class, 'addPost']);

    $router->get('/admin/galerie/directory/([0-9]+)', [\Olymp\Controller\Admin\GalerieDirectory::class, 'list']);

    $router->get('/admin/galerie/directory/edit/([0-9]+)', [\Olymp\Controller\Admin\GalerieDirectory::class, 'edit']);
    $router->post('/admin/galerie/directory/edit/([0-9]+)', [\Olymp\Controller\Admin\GalerieDirectory::class, 'editPost']);

    $router->get('/admin/galerie/directory/remove/([0-9]+)', [\Olymp\Controller\Admin\GalerieDirectory::class, 'remove']);
    $router->post('/admin/galerie/directory/remove/([0-9]+)', [\Olymp\Controller\Admin\GalerieDirectory::class, 'removePost']);

    $router->get('/admin/rozpis/add', [\Olymp\Controller\Admin\Rozpis::class, 'add']);
    $router->post('/admin/rozpis/add', [\Olymp\Controller\Admin\Rozpis::class, 'addPost']);

    $router->get('/admin/rozpis/duplicate/([0-9]+)', [\Olymp\Controller\Admin\Rozpis::class, 'duplicate']);

    $router->get('/admin/rozpis/edit/([0-9]+)', [\Olymp\Controller\Admin\Rozpis::class, 'edit']);
    $router->post('/admin/rozpis/edit/([0-9]+)', [\Olymp\Controller\Admin\Rozpis::class, 'editPost']);

    $router->get('/admin/rozpis/remove/([0-9]+)', [\Olymp\Controller\Admin\Rozpis::class, 'remove']);

    $router->get('/admin/rozpis/detail/([0-9]+)', [\Olymp\Controller\Admin\RozpisDetail::class, 'detail']);
    $router->post('/admin/rozpis/detail/([0-9]+)', [\Olymp\Controller\Admin\RozpisDetail::class, 'detailPost']);

    $router->get('/admin/nabidka/add', [\Olymp\Controller\Admin\Nabidka::class, 'add']);
    $router->post('/admin/nabidka/add', [\Olymp\Controller\Admin\Nabidka::class, 'addPost']);

    $router->get('/admin/nabidka/duplicate/([0-9]+)', [\Olymp\Controller\Admin\Nabidka::class, 'duplicate']);

    $router->get('/admin/nabidka/edit/([0-9]+)', [\Olymp\Controller\Admin\Nabidka::class, 'edit']);
    $router->post('/admin/nabidka/edit/([0-9]+)', [\Olymp\Controller\Admin\Nabidka::class, 'editPost']);

    $router->get('/admin/nabidka/remove/([0-9]+)', [\Olymp\Controller\Admin\Nabidka::class, 'remove']);

    $router->get('/admin/nabidka/detail/([0-9]+)', [\Olymp\Controller\Admin\NabidkaDetail::class, 'detail']);
    $router->post('/admin/nabidka/detail/([0-9]+)', [\Olymp\Controller\Admin\NabidkaDetail::class, 'detailPost']);

    $router->get('/admin/dokumenty/remove/([0-9]+)', [\Olymp\Controller\Admin\Dokumenty::class, 'remove']);
    $router->post('/admin/dokumenty/remove/([0-9]+)', [\Olymp\Controller\Admin\Dokumenty::class, 'removePost']);

    $router->get('/admin/platby/raw', [\Olymp\Controller\Admin\PlatbyRaw::class, 'get']);
    $router->post('/admin/platby/raw', [\Olymp\Controller\Admin\PlatbyRaw::class, 'post']);

    $router->get('/admin/platby/raw/select_columns', [\Olymp\Controller\Admin\PlatbyRaw::class, 'selectColumns']);
    $router->post('/admin/platby/raw/select_columns', [\Olymp\Controller\Admin\PlatbyRaw::class, 'selectColumnsPost']);

    $router->get('/admin/platby/discarded', [\Olymp\Controller\Admin\PlatbyDiscarded::class, 'view']);
    $router->get('/admin/platby/discarded/remove/([0-9]+)', [\Olymp\Controller\Admin\PlatbyDiscarded::class, 'remove']);

    $router->get('/admin/platby/manual/([0-9]+)', [\Olymp\Controller\Admin\PlatbyManual::class, 'get']);
    $router->post('/admin/platby/manual/([0-9]+)', [\Olymp\Controller\Admin\PlatbyManual::class, 'post']);

    $router->get('/admin/platby/items/add', [\Olymp\Controller\Admin\PlatbyItems::class, 'add']);
    $router->post('/admin/platby/items/add', [\Olymp\Controller\Admin\PlatbyItems::class, 'addPost']);

    $router->get('/admin/platby/items/edit/([0-9]+)', [\Olymp\Controller\Admin\PlatbyItems::class, 'edit']);
    $router->post('/admin/platby/items/edit/([0-9]+)', [\Olymp\Controller\Admin\PlatbyItems::class, 'editPost']);

    $router->get('/admin/platby/items/remove/([0-9]+)', [\Olymp\Controller\Admin\PlatbyItems::class, 'remove']);
    $router->post('/admin/platby/items/remove/([0-9]+)', [\Olymp\Controller\Admin\PlatbyItems::class, 'removePost']);

    $router->get('/admin/platby/structure/category', [\Olymp\Controller\Admin\PlatbyCategory::class, 'list']);
    $router->post('/admin/platby/structure/category', [\Olymp\Controller\Admin\PlatbyCategory::class, 'listPost']);

    $router->get('/admin/platby/structure/category/add', [\Olymp\Controller\Admin\PlatbyCategory::class, 'add']);
    $router->post('/admin/platby/structure/category/add', [\Olymp\Controller\Admin\PlatbyCategory::class, 'addPost']);

    $router->get('/admin/platby/structure/category/edit/([0-9]+)', [\Olymp\Controller\Admin\PlatbyCategory::class, 'edit']);
    $router->post('/admin/platby/structure/category/edit/([0-9]+)', [\Olymp\Controller\Admin\PlatbyCategory::class, 'editPost']);

    $router->get('/admin/platby/structure/category/remove/([0-9]+)', [\Olymp\Controller\Admin\PlatbyCategory::class, 'remove']);
    $router->post('/admin/platby/structure/category/remove/([0-9]+)', [\Olymp\Controller\Admin\PlatbyCategory::class, 'removePost']);

    $router->get('/video', [\Olymp\Controller\Video::class, 'get']);

    $router->mount('/old', $router);

    $router->dispatchGlobal();
} catch (AuthorizationException $e) {
    ob_clean();
    http_response_code(403);
    \Render::twig('Error.twig', ['errorCode' => 'authorization']);
} catch (NotFoundException $e) {
    ob_clean();
    syslog(LOG_ERR, "{$_SERVER['REQUEST_URI']}: {$e->getMessage()}");
    http_response_code(404);
    \Render::twig('Error.twig', ['errorCode' => 'not_found']);
} catch (ViewException $e) {
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
    \Render::twig('Error.twig', ['errorCode' => (new ViewException(''))->getErrorFile()]);
}
\Sentry\captureLastError();
