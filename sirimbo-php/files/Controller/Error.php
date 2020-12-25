<?php
namespace Olymp\Controller;

class Error
{
    public static function get()
    {
        $id = implode('', array_map('ucfirst', explode('_', $_GET['id'])));
        $file = "files/Error/$id.inc";
        if (file_exists($file)) {
            ob_start();
            include $file;
            \Message::danger(ob_get_clean());
        } else {
            \Message::danger("Chybová stránka s daným ID nebyla nalezena");
        }
        \Render::twig('Empty.twig', ['header' => 'Chyba']);
    }
}
