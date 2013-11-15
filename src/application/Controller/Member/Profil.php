<?php
namespace TKOlomouc\Controller\Member;

use TKOlomouc\Controller\Member;
use TKOlomouc\Utility\Permissions;
use TKOlomouc\Utility\User;
use TKOlomouc\Utility\Form;
use TKOlomouc\Model\DBUser;
use TKOlomouc\Model\DBSkupiny;
use TKOlomouc\Type\Date;

class Profil extends Member
{
    function __construct() {
        Permissions::checkError('nastenka', P_VIEW);
    }
    function view($id = null) {
        $this->render('src/application/View/Member/Profil/Overview.inc');
    }
    function edit($id = null) {
        $data = DBUser::getUserData(User::getUserID());
        $narozeni = $this->date('narozeni')->getPost();

        if (empty($_POST) || is_object($f = $this->_checkData('edit', $narozeni))) {
            if (empty($_POST)) {
                post("login", User::getUserName());
                post("group", $data["u_group"]);
                post("lock", $data["u_lock"]);
                post("jmeno", $data["u_jmeno"]);
                post("prijmeni", $data["u_prijmeni"]);
                post("pohlavi", $data["u_pohlavi"]);
                post("email", $data["u_email"]);
                post("telefon", $data["u_telefon"]);
                post('narozeni', $data['u_narozeni']);
                post("poznamky", $data["u_poznamky"]);
            } else {
                $this->redirect()->setMessage($f->getMessages());
            }
            $this->render("src/application/View/Member/Profil/PersonalData.inc");
            return;
        }
        DBUser::setUserData(
            User::getUserID(), post('jmeno'), post('prijmeni'),
            post('pohlavi'), post('email'), post('telefon'), (string) $narozeni,
            post('poznamky'), $data['u_group'], $data['u_skupina'],
            $data['u_dancer'], $data['u_lock'], $data['u_ban'], $data['u_system']
        );
        $this->redirect('/member/profil', 'Upraveno');
    }
    function heslo($id = null) {
        if (empty($_POST) || is_object($f = $this->_checkData('heslo'))) {
            if (!empty($_POST)) {
                $this->redirect()->setMessage($f->getMessages());
            }
            $this->render('src/application/View/Member/Profil/NewPassword.inc');
            return;
        }
        DBUser::setPassword(User::getUserID(), User::crypt(post('newpass')));
        $this->redirect('/member/profil', 'Heslo změněno');
    }
    function platby($id = null) {
        $groupsOut = array();
        $groups = DBSkupiny::getSingleWithCategories(User::getSkupina());
        $currentGroup = 0;
        foreach ($groups as $row) {
            if ($currentGroup != $row['pg_id']) {
                $groupsOut[] = array(
                    'name' => ('<span class="big" style="text-decoration:underline;">' . $row['pg_name'] . '</span>'),
                    'type' => (!$row['pg_type'] ? 'Ostatní platby' : 'Členské příspěvky'),
                    'symbol' => '',
                    'amount' => '',
                    'dueDate' => '',
                    'validRange' => ''
                );
                $currentGroup = $row['pg_id'];
            }
            $groupsOut[] = array(
                'name' => $row['pc_name'],
                'type' => '',
                'symbol' => $row['pc_symbol'],
                'amount' => (($row['pc_use_base'] ? ($row['pc_amount'] * $row['pg_base']) : $row['pc_amount']) . ' Kč'),
                'dueDate' => (new Date($row['pc_date_due']))->getDate(Date::FORMAT_SIMPLIFIED),
                'validRange' => ((new Date($row['pc_valid_from']))->getDate(Date::FORMAT_SIMPLIFIED) .
                    ((new Date($row['pc_valid_to']))->isValid() ?
                        (' - ' . (new Date($row['pc_valid_to']))->getDate(Date::FORMAT_SIMPLIFIED)) : ''))
            );
        }
        $skupina = User::getSkupinaData();
        $this->render(
            'src/application/View/Member/Profil/Platby.inc',
            array(
                'colorBox' => getColorBox($skupina['s_color_rgb'], $skupina['s_description']),
                'skupinaData' => $skupina['s_name'],
                'varSymbol' => User::varSymbol(User::getUserID()),
                'zaplacenoText' => User::getZaplaceno() ? 'zaplaceno' : 'nezaplaceno!',
                'platby' => array(),
                'platbyGroups' => $groupsOut
            )
        );
    }
    private function _checkData($action, $narozeni = null) {
        $f = new Form();
        if ($action == 'edit') {
            $f->checkDate((string) $narozeni, 'Neplatné datum narození', 'narozeni');
            $f->checkLength(post('jmeno'), 1, 40, 'Špatná délka jména', 'jmeno');
            $f->checkLength(post('prijmeni'), 1, 40, 'Špatná délka přijmení', 'prijmeni');
            $f->checkInArray(post('pohlavi'), array('m', 'f'), 'Neplatné pohlaví', 'pohlavi');
            $f->checkEmail(post('email'), 'Neplatný formát emailu', 'email');
            $f->checkPhone(post('telefon'), 'Neplatný formát telefoního čísla', 'telefon');
        } elseif ($action == 'heslo') {
            $f->checkPassword(post('newpass'), 'Neplatný formát hesla', 'newpass');
            $f->checkBool(DBUser::checkUser(User::getUserName(), User::crypt(post('oldpass'))),
                'Staré heslo je špatně', 'oldpass');
            $f->checkBool(post('newpass') == post('newpass_confirm'),
                'Nová hesla se neshodují', 'newpass_check');
        }
        return $f->isValid() ? null : $f;
    }
}
?>