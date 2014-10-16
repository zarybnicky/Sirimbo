<?php
class Controller_Admin extends Controller_Abstract
{
    function __construct() {
        Permissions::checkError('nastenka', P_OWNED);
    }
    function view($id = null) {
        $this->render('files/View/Admin/Home.inc');
    }

    public function navbar() {
        return parent::navbar() .
            new Navbar(
                include SETTINGS . '/menu/admin.inner.php',
                false
            );
    }
}
