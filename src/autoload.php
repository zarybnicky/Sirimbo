<?php
require 'Psr4Autoloader.php';

//PSR generic autoloader
$autoloader = new Psr4Autoloader();

$autoloader->addNamespace(
    'TKOlomouc',
    __DIR__ . DIRECTORY_SEPARATOR . 'library'
);
$autoloader->addNamespace(
    'TKOlomouc',
    __DIR__ . DIRECTORY_SEPARATOR . 'application'
);
$autoloader->register();

//Composer autoloader
require '..' . DIRECTORY_SEPARATOR . 'vendor' . DIRECTORY_SEPARATOR . 'autoload.php';