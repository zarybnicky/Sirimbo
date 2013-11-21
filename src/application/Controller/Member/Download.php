<?php
namespace TKOlomouc\Controller\Member;

use TKOlomouc\Controller\ControllerAbstract;
use TKOlomouc\Utility\Permissions;
use TKOlomouc\Model\DBDokumenty;

class Download extends ControllerAbstract
{
    public function __construct()
    {
        Permissions::checkError('dokumenty', P_VIEW);
    }

    public function view($id = null)
    {
        if (!get('id')) {
            $this->redirect('/member/dokumenty');
        }
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
