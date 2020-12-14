<?php
namespace Olymp\Controller;

class Error
{
    public static function get()
    {
        $array = explode('_', $_GET['id']);
        array_walk($array, function (&$str) {
            $str = ucfirst($str);
        });
        $id = implode('', $array);
        $file = ERROR . DIRECTORY_SEPARATOR . $id . '.inc';

        if (file_exists($file)) {
            ob_start();
            include $file;
            $notice = ob_get_clean();
        } else {
            $notice = "Chybová stránka s daným ID nebyla nalezena";
        }
        new \MessageHelper('danger', $notice);
        new \RenderHelper('files/View/Empty.inc', ['header' => 'Chyba']);
    }
}
