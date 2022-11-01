<?php
if ($_ENV['DOMAIN'] ?? '') {
    openlog($_ENV['DOMAIN'], LOG_ODELAY, LOG_USER);
}
date_default_timezone_set('Europe/Paris');
mb_internal_encoding('UTF-8');

define('SENTRY_ENV', $_ENV['DOMAIN'] ?? 'development');
define('DB_CONN_STRING', str_replace($_ENV['DATABASE_URL'], 'postgres:', 'pgsql:'));

$stateDir = $_ENV['STATE_DIR'] ?? getcwd();
define('GALERIE', $stateDir . '/gallery');
define('GALERIE_THUMBS', $stateDir . '/gallery/thumbnails');
define('UPLOADS', $stateDir . '/uploads');
define('CACHE', $stateDir . '/cache');
foreach ([GALERIE, GALERIE_THUMBS, UPLOADS, CACHE] as $path) {
    if (!is_readable($path)) {
        mkdir($path, 0777, true);
    }
}
define('DEFAULT_FROM_MAIL', 'root@tkolymp.cz');
define('DEFAULT_ADMIN_MAIL', 'miroslav.hyza@tkolymp.cz');

define('SMTP_AUTH', $_ENV['SMTP_AUTH'] ?? false);
define('SMTP_TLS', $_ENV['SMTP_TLS'] ?? false);
define('SMTP_HOST', $_ENV['SMTP_HOST']);
define('SMTP_PORT', $_ENV['SMTP_PORT']);
define('SMTP_USER', $_ENV['SMTP_USER'] ?? '');
define('SMTP_PASS', $_ENV['SMTP_PASS'] ?? '');
