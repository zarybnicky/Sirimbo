<?php
class Controller_Member_Download extends Controller_Abstract
{
    public function __construct() {
        Permissions::checkError('dokumenty', P_VIEW);
    }
    public function view($request) {
        if (!get('id'))
            $this->redirect('/member/dokumenty');

        $data = DBDokumenty::getSingleDokument(get('id'));
        $path = $data["d_path"];
        $fileName = $data["d_filename"];
        $file = fopen($path, "rb");

        header('Pragma: no-cache');
        header('Content-Type: application/octet-stream');
        header('Content-Disposition: inline; filename="' . ($fileName) . '"');
        fpassthru($file);
        fclose($file);
    }
}
