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

require 'files/autoload.php';
require 'files/Core/settings.php';
require 'files/Core/utils.php';
require 'files/Core/debug.php';
require 'files/Core/log.php';
require 'files/Core/request.php';
require 'files/Controller/Interface.php';
require 'files/Controller/Abstract.php';

define('TISK', (get('view') == 'tisk') ? true : false);

if (TISK) {
    ;
} elseif (!session('page_id')) {
    session('page_id', server('REQUEST_URI'));
} elseif (!session('referer_id') || server('REQUEST_URI') != session('page_id')) {
    session('referer_id', session('page_id'));
    session('page_id', server('REQUEST_URI'));
}

Request::setDefault('home');
Request::setURI(server('REQUEST_URI'));
Request::setReferer(session('referer_id'));

if (session('login') === null) {
    if (post('login') && post('pass')) {
        post('pass', User::crypt(post('pass')));

        if (!User::login(post('login'), post('pass'))) {
            Helper::instance()->redirect('/login', 'Špatné jméno nebo heslo!', true);
        } elseif (get('return')) {
            Helper::instance()->redirect(get('return'));
        } else {
            Helper::instance()->redirect('/member/nastenka');
        }
    }
} else {
    User::loadUser(session('id'));
    if (session('invalid_data') === '1'
        && Request::getURI() !== 'member/profil/edit'
        && Request::getURI() !== 'logout'
    ) {
        Helper::instance()->redirect(
            '/member/profil/edit',
            'Prosím vyplňte požadované údaje.',
            true
        );
    }
}

$d = new Dispatcher();
try {
    $d->dispatch(Request::getLiteralURI(), Request::getAction(), Request::getID());
} catch (ViewException $e) {
    Log::write($e->getMessage() . ' (' . $e->getFile() . ':' . $e->getLine() . ")\n" . $e->getTraceAsString());
    ob_clean();
    Helper::instance()->redirect('/error?id=' . $e->getErrorFile());
} catch (Exception $e) {
    Log::write($e->getMessage() . ' (' . $e->getFile() . ':' . $e->getLine() . ")\n" . $e->getTraceAsString());
    ob_clean();
    Helper::instance()->redirect('/error?id=' . (new ViewException(''))->getErrorFile());
}
