<?php
class Controller_Member_Download
{
    public function view($request)
    {
        \Permissions::checkError('dokumenty', P_VIEW);
        if (!$_GET['id']) {
            new \RedirectHelper('/member/dokumenty');
        }

        $data = \DBDokumenty::getSingleDokument($_GET['id']);
        $path = $data['d_path'];
        if (!is_file($path) || !($file = fopen($path, 'rb'))) {
            new \MessageHelper('warning', 'Soubor nebyl nalezen.');
            return new \RedirectHelper('/member/dokumenty');
        }

        header('Pragma: no-cache');
        header('Content-Type: application/octet-stream');
        header('Content-Disposition: inline; filename="' . $data['d_filename'] . '"');
        fpassthru($file);
        fclose($file);
    }
}
