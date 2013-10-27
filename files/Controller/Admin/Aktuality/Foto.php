<?php
require_once 'files/Controller/Admin/Aktuality.php';
class Controller_Admin_Aktuality_Foto extends Controller_Admin_Aktuality
{
    function __construct() {
        Permissions::checkError('aktuality', P_OWNED);
    }
    function view($id = null) {

    }
}
?>