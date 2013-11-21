<?php
namespace TKOlomouc\Controller\Admin\Aktuality;

use TKOlomouc\Controller\Admin\Aktuality;
use TKOlomouc\Utility\Permissions;
use TKOlomouc\Utility\Response;

class Foto extends Aktuality
{
    public function __construct()
    {
        Permissions::checkError('aktuality', P_OWNED);
    }

    public function view($id = null)
    {
        Response::redirect('/admin/aktuality', 'Ještě není implementováno...');
    }
}
