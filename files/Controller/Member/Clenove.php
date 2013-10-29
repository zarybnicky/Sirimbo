<?php
require_once 'files/Controller/Member.php';
class Controller_Member_Clenove extends Controller_Member
{
    function __construct() {
        Permissions::checkError('users', P_VIEW);
    }
    function view($id = null) {
        if (!$id || !($data = DBUser::getUserData($id)))
            $this->redirect('/member/clenove/structure');

        $this->render(
            'files/View/Member/Clenove/Single.inc',
            array(
                'fullName' => $data['u_prijmeni'] . ', ' . $data['u_jmeno'],
                'email' => $data['u_email'],
                'telefon' => $data['u_telefon'],
                'referer' => Request::getReferer()
            )
        );
    }
    function skupiny($id = null) {
        $currentID = 0;
        $currentKey = 0;
        $data = DBUser::getUsersWithSkupinaPlatby();
        $skupiny = array();
        foreach ($data as $item) {
            if ($item['s_id'] != $currentID) {
                $index = 0;
                $currentID = $item['s_id'];
                $currentKey = count($skupiny) - 1;
                $skupiny[$currentKey] = array(
                    'header' => '<h3>' . getColorBox($item['s_color_rgb'], $item['s_description']) .
                        '&nbsp;&nbsp;' . $item['s_name'] . '</h2>',
                    'description' => $item['s_description'],
                    'userCount' => 0
                );
            }
            $skupiny[$currentKey]['userCount']++;
        }
        $this->render(
            'files/View/Member/Clenove/SkupinyList.inc',
            array(
                'data' => $skupiny
            )
        );
    }
    function seznam($id = null) {
        $index = 0;
        $data = DBUser::getActiveUsers();
        foreach ($data as &$item) {
            $new_data = array(
                'index' => ++$index . '.',
                'fullName' => '<a href="/member/clenove/' . $item['u_id'] . '">' .
                    '<img src="/style/person-small.png" alt="' . $item['u_jmeno'] . ' ' . $item['u_prijmeni'] .
                    '" style="margin-bottom:-2px"/>' .
                    '</a>' .
                    '&nbsp;' . $item['u_prijmeni'] . ', ' . $item['u_jmeno']
            );
            $item = $new_data;
        }
        $this->render(
            'files/View/Member/Clenove/UserList.inc',
            array(
                'data' => $data
            )
        );
    }
    function structure($id = null) {
        $data = DBUser::getUsersWithSkupinaPlatby();
        $skupiny = array();
        $index = 0;
        $currentID = 0;
        $currentKey = 0;
        foreach ($data as $item) {
            if ($item['s_id'] != $currentID) {
                $index = 0;
                $currentID = $item['s_id'];
                $currentKey = count($skupiny) - 1;
                $skupiny[$currentKey] = array();
                $skupiny[$currentKey]['info'] = array(
                    'header' => '<h3>' . getColorBox($item['s_color_rgb'], $item['s_description']) .
                        '&nbsp;&nbsp;' . $item['s_name'] . '</h2>'
                );
                $skupiny[$currentKey]['users'] = array();
            }
            $skupiny[$currentKey]['users'][] = array(
                'index' => ++$index . '.',
                'fullName' => '<a href="/member/clenove/' . $item['u_id'] . '">' .
                    '<img src="/style/person-small.png" alt="' . $item['u_jmeno'] . ' ' . $item['u_prijmeni'] .
                    '" style="margin-bottom:-2px"/>' .
                    '</a>' .
                    '&nbsp;' . $item['u_prijmeni'] . ', ' . $item['u_jmeno'],
                'hasPaid' => $item['pi_id'] ? '<span style="font-weight:bold;color:green;">ANO</span>' :
                    '<span style="font-weight:bold;color:red;">NE</span>'
            );
        }
        $leftCount = 0;
        $rightCount = 0;
        foreach ($skupiny as &$skupina) {
            $skupina['info']['count'] = count($skupina['users']);
            if ($rightCount >= $leftCount) {
                $skupina['info']['align'] = 'left';
                $leftCount += ($skupina['info']['count'] + 1.5);
            } else {
                $skupina['info']['align'] = 'right';
                $rightCount += ($skupina['info']['count'] + 1.5);
            }
        }
        $this->render(
            'files/View/Member/Clenove/Structure.inc',
            array(
                'data' => $skupiny
            )
        );
    }
}
?>