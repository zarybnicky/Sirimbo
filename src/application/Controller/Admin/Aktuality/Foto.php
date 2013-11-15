<?php
namespace TKOlomouc\Controller\Admin\Aktuality;

use TKOlomouc\Controller\Admin\Aktuality;
use TKOlomouc\Utility\Permissions;

class Foto extends Aktuality
{
    function __construct() {
        Permissions::checkError('aktuality', P_OWNED);
    }
    function view($id = null) {

    }
}
?>