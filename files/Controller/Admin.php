<?php
class Controller_Admin extends Controller_Abstract
{
    public function __construct() {
        Permissions::checkError('nastenka', P_OWNED);
    }

    public function view($request) {
        $this->render('files/View/Admin/Home.inc');
    }

    public function navbar() {
        return array_merge(parent::navbar(), [include SETTINGS . '/menu/admin.inner.php']);
    }
}
