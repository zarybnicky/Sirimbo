<?php
require_once 'files/Controller/Admin/Akce.php';
class Controller_Admin_Akce_Detail extends Controller_Admin_Akce
{
    function __construct() {
        Permissions::checkError('akce', P_OWNED);
    }
    function view($id = null) {
        if (!$id || !($akce = DBAkce::getSingleAkce($id)))
            $this->redirect('/admin/akce', 'Akce s takovým ID neexistuje');

        $items = DBAkce::getAkceItems($id);
        $users = DBUser::getActiveUsers();

        if (!empty($_POST)) {
            if (post("remove") > 0) {
                DBAkce::removeAkceItem(post("remove"));
                $items = DBAkce::getAkceItems($id);
            }

            foreach ($items as $item) {
                $item_id = $item["ai_id"];
                $user = post($item_id . '-user');

                if ($user != $item["ai_user"]) {
                    $data = DBUser::getUserData($user);
                    list($year) = explode('-', $data['u_narozeni']);
                    DBAkce::editAkceItem($item_id, $user, $year);
                }
            }
            $items = DBAkce::getAkceItems($id);

            if (is_numeric(post("add-user"))) {
                $user = post("add-user");
                $data = DBUser::getUserData($user);
                list($year) = explode('-', $data['u_narozeni']);

                DBAkce::addAkceItem($id, $user, $year);
                post('add-user', -1);
                $items = DBAkce::getAkceItems($id);
            }
        }
        $new_data = array(
                'id' => $akce['a_id'],
                'jmeno' => $akce['a_jmeno'],
                'kde' => $akce['a_kde'],
                'datum' => formatDate($akce['a_od']) .
                (($akce['a_od'] != $akce['a_do']) ? ' - ' . formatDate($akce['a_do']) : ''),
                'kapacita' => $akce['a_kapacita'],
                'volno' => $akce['a_kapacita'] - count($items),
                'showForm' => Permissions::check('akce', P_MEMBER) && !$akce['a_lock'],
                'canEdit' => Permissions::check('akce', P_OWNED),
                'info' => nl2br($akce['a_info'])
        );
        $akce = $new_data;
        $this->render("files/Admin/AkceDetail/Display.inc", array(
                'data' => $akce,
                'users' => $users,
                'items' => $items
        ));
    }
}
?>