<?php
namespace Olymp\Controller;

class Error
{
    public static function get()
    {
        \Render::twig('Error.twig', ['errorCode' => $_GET['id']]);
    }
}
