<?php
$core = __DIR__ . DIRECTORY_SEPARATOR . 'Core' . DIRECTORY_SEPARATOR;
set_include_path(
    $core . PATH_SEPARATOR .
    $core . 'database' . PATH_SEPARATOR .
    $core . 'display' . PATH_SEPARATOR .
    $core . 'helpers' . PATH_SEPARATOR .
    $core . 'types' . PATH_SEPARATOR .
    get_include_path()
);
spl_autoload_extensions('.php');
spl_autoload_register();
