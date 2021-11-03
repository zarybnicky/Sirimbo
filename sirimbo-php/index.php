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

Sentry\init([
    'environment' => SENTRY_ENV,
    'dsn' => 'https://943ee3e7e7044524b2ee8413a957e14f@o775093.ingest.sentry.io/5796825',
    'traces_sample_rate' => 1,
]);

require 'files/Core/settings.php';

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

function makeRouter()
{
    $router = new \Olymp\Router('Olymp.Controller');

    $router->get('/', '@Home::get');
    $router->get('/home', '@Home::get');
    $router->get('/error', '@Error::get');
    $router->get('/video', '@Video::get');

    $router->get('/kontakt', '@StaticContent::kontakt');
    $router->get('/oklubu/klubovi-treneri', '@StaticContent::klubovi');
    $router->get('/oklubu/externi-treneri', '@StaticContent::externi');
    $router->get('/oklubu/saly', '@StaticContent::saly');

    $router->get('/aktualne', '@Aktualne::list');
    $router->get('/aktualne/([0-9]+)', '@Aktualne::single');
    $router->get('/fotogalerie', '@Fotogalerie::root');
    $router->get('/fotogalerie/([0-9]+)', '@Fotogalerie::directory');
    $router->get('/fotogalerie/foto/([0-9]+)', '@Fotogalerie::single');
    $router->get('/fotogalerie/([0-9]+)/foto/([0-9]+)', '@Fotogalerie::singleWithDir');

    $router->get('/login', '@Member::login');
    $router->get('/nopassword', '@Nopassword::get');
    $router->post('/nopassword', '@Nopassword::post');
    $router->get('/registrace', '@Registrace::get');
    $router->post('/registrace', '@Registrace::post');

    $router->get('/member', '@Member::get');
    $router->redirect('/member/home', '/member');
    $router->get('/member/akce', '@Member.Akce::list');
    $router->post('/member/akce', '@Member.Akce::listPost');
    $router->get('/member/akce/([0-9]+)', '@Member.Akce::single');
    $router->redirect('/member/rozpis', '/member/treninky');
    $router->redirect('/member/nabidka', '/member/treninky');
    $router->get('/member/treninky', '@Member.Schedule::get');
    $router->post('/member/treninky', '@Member.Schedule::post');
    $router->get('/member/dokumenty', '@Member::dokumenty');
    $router->get('/member/download', '@Member::download');
    $router->get('/member/clenove', '@Member.Clenove::structure');
    $router->redirect('/member/clenove/structure', '/member/clenove');
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
    $router->get('/member/profil/par/partner', '@Member.ProfilPar::partner');
    $router->post('/member/profil/par/partner', '@Member.ProfilPar::partnerPost');
    $router->post('/member/profil/par/zadost', '@Member.ProfilPar::zadost');

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

    $router->get('/admin/aktuality', '@Admin.Aktuality::list');
    $router->get('/admin/aktuality/add', '@Admin.Aktuality::add');
    $router->post('/admin/aktuality/add', '@Admin.Aktuality::addPost');
    $router->get('/admin/aktuality/edit/([0-9]+)', '@Admin.Aktuality::edit');
    $router->post('/admin/aktuality/edit/([0-9]+)', '@Admin.Aktuality::editPost');
    $router->get('/admin/aktuality/remove/([0-9]+)', '@Admin.Aktuality::remove');
    $router->post('/admin/aktuality/remove/([0-9]+)', '@Admin.Aktuality::removePost');
    $router->get('/admin/aktuality/foto/([0-9]+)', '@Admin.Aktuality::foto');
    $router->post('/admin/aktuality/foto/([0-9]+)', '@Admin.Aktuality::fotoPost');

    $router->get('/admin/video', '@Admin.Video::orphan');
    $router->get('/admin/video/orphan', '@Admin.Video::orphan');
    $router->get('/admin/video/title', '@Admin.Video::title');
    $router->post('/admin/video/title', '@Admin.Video::titlePost');
    $router->get('/admin/video/playlist', '@Admin.Video::playlistList');
    $router->get('/admin/video/playlist/([0-9]+)', '@Admin.Video::playlist');
    $router->get('/admin/video/add', '@Admin.Video::add');
    $router->post('/admin/video/add', '@Admin.Video::addPost');
    $router->get('/admin/video/edit/([0-9]+)', '@Admin.Video::edit');
    $router->post('/admin/video/edit/([0-9]+)', '@Admin.Video::editPost');
    $router->get('/admin/video/remove/([0-9]+)', '@Admin.Video::remove');
    $router->post('/admin/video/remove/([0-9]+)', '@Admin.Video::removePost');

    $router->get('/admin/video/source', '@Admin.VideoSource::list');
    $router->get('/admin/video/source/add', '@Admin.VideoSource::add');
    $router->post('/admin/video/source/add', '@Admin.VideoSource::addPost');
    $router->get('/admin/video/source/edit/([0-9]+)', '@Admin.VideoSource::edit');
    $router->post('/admin/video/source/edit/([0-9]+)', '@Admin.VideoSource::editPost');
    $router->get('/admin/video/source/remove/([0-9]+)', '@Admin.VideoSource::remove');
    $router->post('/admin/video/source/remove/([0-9]+)', '@Admin.VideoSource::removePost');

    $router->get('/admin/nastenka', '@Admin.Nastenka::list');
    $router->get('/admin/nastenka/add', '@Admin.Nastenka::add');
    $router->post('/admin/nastenka/add', '@Admin.Nastenka::addPost');
    $router->get('/admin/nastenka/edit/([0-9]+)', '@Admin.Nastenka::edit');
    $router->post('/admin/nastenka/edit/([0-9]+)', '@Admin.Nastenka::editPost');
    $router->get('/admin/nastenka/remove/([0-9]+)', '@Admin.Nastenka::remove');
    $router->post('/admin/nastenka/remove/([0-9]+)', '@Admin.Nastenka::removePost');

    $router->get('/admin/users', '@Admin.Users::list');
    $router->post('/admin/users', '@Admin.Users::listPost');
    $router->get('/admin/users/add', '@Admin.Users::add');
    $router->post('/admin/users/add', '@Admin.Users::addPost');
    $router->get('/admin/users/edit/([0-9]+)', '@Admin.Users::edit');
    $router->post('/admin/users/edit/([0-9]+)', '@Admin.Users::editPost');
    $router->get('/admin/users/sign-as/([0-9]+)', '@Admin.Users::signAs');
    $router->get('/admin/users/remove/([0-9]+)', '@Admin.Users::remove');
    $router->post('/admin/users/remove/([0-9]+)', '@Admin.Users::removePost');
    $router->get('/admin/users/getMsmtCsv', '@Admin.Users::getMsmtCsv');
    $router->get('/admin/users/unconfirmed', '@Admin.Users::unconfirmed');
    $router->post('/admin/users/unconfirmed', '@Admin.Users::unconfirmedPost');
    $router->get('/admin/users/duplicate', '@Admin.Users::duplicate');
    $router->get('/admin/users/statistiky', '@Admin.Users::statistiky');

    $router->get('/admin/galerie', '@Admin.Galerie::list');
    $router->get('/admin/galerie/file/upload', '@Admin.GalerieFile::upload');
    $router->post('/admin/galerie/file/upload', '@Admin.GalerieFile::uploadPost');
    $router->get('/admin/galerie/file/edit/([0-9]+)', '@Admin.GalerieFile::edit');
    $router->post('/admin/galerie/file/edit/([0-9]+)', '@Admin.GalerieFile::editPost');
    $router->get('/admin/galerie/file/remove/([0-9]+)', '@Admin.GalerieFile::remove');
    $router->post('/admin/galerie/file/remove/([0-9]+)', '@Admin.GalerieFile::removePost');
    $router->get('/admin/galerie/directory/add', '@Admin.GalerieDirectory::add');
    $router->post('/admin/galerie/directory/add', '@Admin.GalerieDirectory::addPost');
    $router->get('/admin/galerie/directory/([0-9]+)', '@Admin.GalerieDirectory::list');
    $router->get('/admin/galerie/directory/edit/([0-9]+)', '@Admin.GalerieDirectory::edit');
    $router->post('/admin/galerie/directory/edit/([0-9]+)', '@Admin.GalerieDirectory::editPost');
    $router->get('/admin/galerie/directory/remove/([0-9]+)', '@Admin.GalerieDirectory::remove');
    $router->post('/admin/galerie/directory/remove/([0-9]+)', '@Admin.GalerieDirectory::removePost');

    $router->get('/admin/rozpis', '@Admin.Rozpis::list');
    $router->post('/admin/rozpis', '@Admin.Rozpis::listPost');
    $router->get('/admin/rozpis/add', '@Admin.Rozpis::add');
    $router->post('/admin/rozpis/add', '@Admin.Rozpis::addPost');
    $router->get('/admin/rozpis/duplicate/([0-9]+)', '@Admin.Rozpis::duplicate');
    $router->get('/admin/rozpis/edit/([0-9]+)', '@Admin.Rozpis::edit');
    $router->post('/admin/rozpis/edit/([0-9]+)', '@Admin.Rozpis::editPost');
    $router->get('/admin/rozpis/remove/([0-9]+)', '@Admin.Rozpis::remove');
    $router->get('/admin/rozpis/detail/([0-9]+)', '@Admin.RozpisDetail::detail');
    $router->post('/admin/rozpis/detail/([0-9]+)', '@Admin.RozpisDetail::detailPost');

    $router->get('/admin/nabidka', '@Admin.Nabidka::list');
    $router->post('/admin/nabidka', '@Admin.Nabidka::listPost');
    $router->get('/admin/nabidka/add', '@Admin.Nabidka::add');
    $router->post('/admin/nabidka/add', '@Admin.Nabidka::addPost');
    $router->get('/admin/nabidka/duplicate/([0-9]+)', '@Admin.Nabidka::duplicate');
    $router->get('/admin/nabidka/edit/([0-9]+)', '@Admin.Nabidka::edit');
    $router->post('/admin/nabidka/edit/([0-9]+)', '@Admin.Nabidka::editPost');
    $router->get('/admin/nabidka/remove/([0-9]+)', '@Admin.Nabidka::remove');
    $router->get('/admin/nabidka/detail/([0-9]+)', '@Admin.NabidkaDetail::detail');
    $router->post('/admin/nabidka/detail/([0-9]+)', '@Admin.NabidkaDetail::detailPost');

    $router->get('/admin/dokumenty', '@Admin.Dokumenty::list');
    $router->post('/admin/dokumenty', '@Admin.Dokumenty::listPost');
    $router->get('/admin/dokumenty/edit/([0-9]+)', '@Admin.Dokumenty::edit');
    $router->post('/admin/dokumenty/edit/([0-9]+)', '@Admin.Dokumenty::editPost');
    $router->get('/admin/dokumenty/remove/([0-9]+)', '@Admin.Dokumenty::remove');
    $router->post('/admin/dokumenty/remove/([0-9]+)', '@Admin.Dokumenty::removePost');

    $router->get('/admin/pary', '@Admin.Pary::list');
    $router->post('/admin/pary', '@Admin.Pary::listPost');
    $router->get('/admin/pary/edit/([0-9]+)', '@Admin.Pary::edit');
    $router->post('/admin/pary/edit/([0-9]+)', '@Admin.Pary::editPost');
    $router->get('/admin/pary/remove/([0-9]+)', '@Admin.Pary::remove');

    $router->get('/admin/permissions', '@Admin.Permissions::list');
    $router->get('/admin/permissions/add', '@Admin.Permissions::add');
    $router->post('/admin/permissions/add', '@Admin.Permissions::addPost');
    $router->get('/admin/permissions/edit/([0-9]+)', '@Admin.Permissions::edit');
    $router->post('/admin/permissions/edit/([0-9]+)', '@Admin.Permissions::editPost');
    $router->get('/admin/permissions/remove/([0-9]+)', '@Admin.Permissions::remove');
    $router->post('/admin/permissions/remove/([0-9]+)', '@Admin.Permissions::removePost');

    $router->get('/admin/skupiny', '@Admin.Skupiny::list');
    $router->get('/admin/skupiny/add', '@Admin.Skupiny::add');
    $router->post('/admin/skupiny/add', '@Admin.Skupiny::addPost');
    $router->get('/admin/skupiny/edit/([0-9]+)', '@Admin.Skupiny::edit');
    $router->post('/admin/skupiny/edit/([0-9]+)', '@Admin.Skupiny::editPost');
    $router->get('/admin/skupiny/remove/([0-9]+)', '@Admin.Skupiny::remove');
    $router->post('/admin/skupiny/remove/([0-9]+)', '@Admin.Skupiny::removePost');

    $router->get('/admin/platby', '@Admin.Platby::overview');
    $router->get('/admin/platby/structure', '@Admin.Platby::structure');
    $router->get('/admin/platby/raw', '@Admin.PlatbyRaw::get');
    $router->post('/admin/platby/raw', '@Admin.PlatbyRaw::post');
    $router->get('/admin/platby/raw/select_columna', '@Admin.PlatbyRaw::selectColumns');
    $router->post('/admin/platby/raw/select_columna', '@Admin.PlatbyRaw::selectColumnsPost');
    $router->get('/admin/platby/discarded', '@Admin.PlatbyDiscarded::view');
    $router->get('/admin/platby/discarded/remove/([0-9]+)', '@Admin.PlatbyDiscarded::remove');
    $router->get('/admin/platby/manual', '@Admin.PlatbyManual::query');
    $router->get('/admin/platby/manual/([0-9]+)', '@Admin.PlatbyManual::get');
    $router->post('/admin/platby/manual/([0-9]+)', '@Admin.PlatbyManual::post');

    $router->get('/admin/platby/items', '@Admin.PlatbyItems::list');
    $router->get('/admin/platby/items/add', '@Admin.PlatbyItems::add');
    $router->post('/admin/platby/items/add', '@Admin.PlatbyItems::addPost');
    $router->get('/admin/platby/items/edit/([0-9]+)', '@Admin.PlatbyItems::edit');
    $router->post('/admin/platby/items/edit/([0-9]+)', '@Admin.PlatbyItems::editPost');
    $router->get('/admin/platby/items/remove/([0-9]+)', '@Admin.PlatbyItems::remove');
    $router->post('/admin/platby/items/remove/([0-9]+)', '@Admin.PlatbyItems::removePost');

    $router->get('/admin/platby/structure/group', '@Admin.PlatbyGroup::list');
    $router->get('/admin/platby/structure/group/add', '@Admin.PlatbyGroup::add');
    $router->post('/admin/platby/structure/group/add', '@Admin.PlatbyGroup::addPost');
    $router->get('/admin/platby/structure/group/edit/([0-9]+)', '@Admin.PlatbyGroup::edit');
    $router->post('/admin/platby/structure/group/edit/([0-9]+)', '@Admin.PlatbyGroup::editPost');
    $router->get('/admin/platby/structure/group/remove/([0-9]+)', '@Admin.PlatbyGroup::remove');
    $router->post('/admin/platby/structure/group/remove/([0-9]+)', '@Admin.PlatbyGroup::removePost');

    $router->get('/admin/platby/structure/category', '@Admin.PlatbyCategory::list');
    $router->post('/admin/platby/structure/category', '@Admin.PlatbyCategory::listPost');
    $router->get('/admin/platby/structure/category/add', '@Admin.PlatbyCategory::add');
    $router->post('/admin/platby/structure/category/add', '@Admin.PlatbyCategory::addPost');
    $router->get('/admin/platby/structure/category/edit/([0-9]+)', '@Admin.PlatbyCategory::edit');
    $router->post('/admin/platby/structure/category/edit/([0-9]+)', '@Admin.PlatbyCategory::editPost');
    $router->get('/admin/platby/structure/category/remove/([0-9]+)', '@Admin.PlatbyCategory::remove');
    $router->post('/admin/platby/structure/category/remove/([0-9]+)', '@Admin.PlatbyCategory::removePost');

    return $router;
}
