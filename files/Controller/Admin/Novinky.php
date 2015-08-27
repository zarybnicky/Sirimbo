<?php
require_once 'files/Controller/Admin.php';
class Controller_Admin_Novinky extends Controller_Admin
{
    public function __construct() {
        Permissions::checkError('nastenka', P_OWNED);
    }
    public function view($request) {
        if (!is_numeric($request->get('id'))) {
            $this->redirect("/member/home", "Novinka s daným ID neexistuje");
        }
        DBNovinky::removeNovinka($request->get('id'));
        $this->redirect("/member/home", "Novinka odstraněna");
    }
}
