<?php
namespace Olymp\Controller;

class Error
{
    public static function get()
    {
        \Render::twig('Error.twig', [
            'header' => 'Chyba',
            'errorCode' => $_GET['id'],
        ]);
    }
}
