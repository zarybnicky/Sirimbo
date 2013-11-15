<?php
namespace TKOlomouc\Controller\Admin;

use TKOlomouc\Controller\Admin;
use TKOlomouc\Utility\Permissions;
use TKOlomouc\Model\DBNovinky;

class Novinky extends Admin
{
    function __construct() {
        Permissions::checkError('nastenka', P_OWNED);
    }
    function view($id = null) {
        if (!is_numeric(get('id')))
            $this->redirect("/member/home", "Novinka s daným ID neexistuje");
        DBNovinky::removeNovinka(get('id'));
        $this->redirect("/member/home", "Novinka odstraněna");
    }
}
?>