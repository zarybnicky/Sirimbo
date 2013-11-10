<?php
$real_name = realpath(basename($_SERVER['SCRIPT_NAME']));
define('ROOT', substr($real_name, 0, strpos($real_name, basename($_SERVER['SCRIPT_NAME'])) - 1));
unset($real_name);

define('LOG', ROOT . DIRECTORY_SEPARATOR . 'log');
define('LOG', LOG_DIRECTORY . DIRECTORY_SEPARATOR . 'error.log');
define('DEBUG_LOG', LOG_DIRECTORY . DIRECTORY_SEPARATOR . 'debug.log');
define('PHP_LOG', LOG_DIRECTORY . DIRECTORY_SEPARATOR . 'php.log');

define('PUBLIC_DIR', ROOT . DIRECTORY_SEPARATOR . 'public');
define('GALERIE', PUBLIC_DIR . DIRECTORY_SEPARATOR . 'galerie');
define('GALERIE_THUMBS', GALERIE . DIRECTORY_SEPARATOR . 'thumbnails');

define('SOURCE', ROOT . DIRECTORY_SEPARATOR . 'src');
define('LIBRARY', SOURCE . DIRECTORY_SEPARATOR . 'library');
define('APPLICATION', SOURCE . DIRECTORY_SEPARATOR . 'application');
define('CONTROLLER_DIRECTORY', APPLICATION . DIRECTORY_SEPARATOR . 'Controller');
define('VIEW_DIRECTORY', APPLICATION . DIRECTORY_SEPARATOR . 'View');
define('SETTINGS', APPLICATION . DIRECTORY_SEPARATOR . 'settings');

define('STATIC_DIR', APPLICATION . DIRECTORY_SEPARATOR . 'Static');
define('HEADER', STATIC_DIR . 'Header.inc');
define('FOOTER', STATIC_DIR . DIRECTORY_SEPARATOR . 'Footer.inc');
define('HEADER_TISK', STATIC_DIR . DIRECTORY_SEPARATOR . 'HeaderTisk.inc');
define('FOOTER_TISK', STATIC_DIR . DIRECTORY_SEPARATOR . 'FooterTisk.inc');

define('DEFAULT_FROM_MAIL', 'TK Olymp.cz <noreply@tkolymp.cz>');
define('DEFAULT_ADMIN_MAIL', 'tkolymp@tkolymp.cz');

define('NABOR', '0');

define('TISK', (isset($_GET['view']) && $_GET['view'] == 'tisk') ? true : false);

//-----Hodnoceni paru-----//
define('AMEND_Z', '0.2');
define('AMEND_H', '0.5');
define('AMEND_D', '1.0');
define('AMEND_C', '1.6');
define('AMEND_B', '2.1');
define('AMEND_A', '2.7');
define('AMEND_M', '3.4');

define('BONUS_Z', '0');
define('BONUS_H', '80');    //400*AMEND_Z + BONUS_Z
define('BONUS_D', '280');    //400*AMEND_H + BONUS_H
define('BONUS_C', '680');    //400*AMEND_D + BONUS_D
define('BONUS_B', '1320');    //400*AMEND_C + BONUS_C
define('BONUS_A', '2160');    //400*AMEND_B + BONUS_B
define('BONUS_M', '3240');    //400*AMEND_A + BONUS_A

//-----Aliasy chyb-----//
define('ER_AUTHORIZATION', 'authorization');
define('ER_CORRUPT_DATA', 'corrupt_data');
define('ER_BAN', 'ban');
define('ER_DATABASE', 'database');
define('ER_DATABASE_CONNECTION', 'database_connection');
define('ER_PREPARING_FILE', 'preparing_file');
define('ER_CORRUPT_KEY_FILE', 'corrupt_key_file');
define('ER_NOT_APPROVED', 'not_approved');
define('ER_NOT_FOUND_RIGHT', 'not_found_right');
define('ER_NOT_POSSIBLE', 'not_possible');
define('ER_SCRIPT_FATAL', 'script_fatal');

define('NOVINKY_COUNT', 10);
define('AKTUALITY_CLANKY', 1);
define('AKTUALITY_VIDEA', 2);
define('AKTUALITY_KRATKE', 3);
define('AKTUALITY_PREVIEW', 200);
define('THUMBNAIL_MAX', 150);

//-----Ciselne hodnoty urovni uzivatelu-----//
define('L_ALL', '-1');
define('L_UNCONFIRMED', '0');

define('P_NONE', 1);
define('P_VIEW', 2);
define('P_MEMBER', 4);
define('P_OWNED', 8);
define('P_ADMIN', 16);
