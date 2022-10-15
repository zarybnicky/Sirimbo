<?php
namespace Olymp\Controller;

class Member
{
    public static function download()
    {
        \Permissions::checkError('dokumenty', P_VIEW);
        if (!$_GET['id']) {
            echo "Soubor nebyl nalezen.";
            http_response_code(400);
            return;
        }

        $data = \DBDokumenty::getSingleDokument($_GET['id']);
        $path = $data['d_path'];
        if (!is_file($path) || !($file = fopen($path, 'rb'))) {
            echo "Neplatný soubor.";
            http_response_code(404);
            return;
        }

        header('Pragma: no-cache');
        header('Content-Type: application/octet-stream');
        header('Content-Disposition: inline; filename="' . $data['d_filename'] . '"');
        fpassthru($file);
        fclose($file);
    }
}
