<?php
class Controller_Member_Download extends Controller_Abstract
{
    public function __construct()
    {
        Permissions::checkError('dokumenty', P_VIEW);
    }

    public function view($request)
    {
        if (!$request->get('id')) {
            $this->redirect('/member/dokumenty');
        }

        $data = DBDokumenty::getSingleDokument($request->get('id'));
        $path = $data['d_path'];
        if (!is_file($path) || !($file = fopen($path, 'rb'))) {
            $this->redirect()->warning('Soubor nebyl nalezen.');
            return $this->redirect('/member/dokumenty');
        }

        header('Pragma: no-cache');
        header('Content-Type: application/octet-stream');
        header('Content-Disposition: inline; filename="' . $data['d_filename'] . '"');
        fpassthru($file);
        fclose($file);
    }
}