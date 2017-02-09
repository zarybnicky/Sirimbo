<?php
require_once 'files/Controller/Member.php';
class Controller_Member_Clenove extends Controller_Member
{
    public function __construct() {
        Permissions::checkError('users', P_VIEW);
    }
    public function view($request) {
        $id = $request->getId();
        if (!$id || !($data = DBUser::getUserData($id)))
            $this->redirect('/member/clenove/structure');

        $this->render(
            'files/View/Member/Clenove/Single.inc',
            array(
                'fullName' => $data['u_prijmeni'] . ', ' . $data['u_jmeno'],
                'email' => $data['u_email'],
                'telefon' => $data['u_telefon'],
                'referer' => $request->getReferer(),
                'uri' => $request->getLiteralURI()
            )
        );
    }
    public function skupiny($request) {
        $currentID = -1;
        $currentKey = 0;
        $data = DBUser::getUsersWithSkupinaPlatby();
        $skupiny = array();
        foreach ($data as $item) {
            if ($item['s_id'] != $currentID) {
                $index = 0;
                $currentID = $item['s_id'];
                $currentKey = count($skupiny);
                $skupiny[$currentKey] = array(
                    'header' => new Tag(
                        'h3',
                        array(),
                        $this->colorbox($item['s_color_rgb'], $item['s_description'])->render(),
                        '&nbsp;&nbsp;',
                        $item['s_name']
                    ),
                    'description' => $item['s_description'],
                    'userCount' => 0
                );
            }
            $skupiny[$currentKey]['userCount']++;
        }
        $this->render(
            'files/View/Member/Clenove/SkupinyList.inc',
            array(
                'data' => $skupiny,
                'uri' => $request->getLiteralURI()
            )
        );
    }
    public function seznam($request) {
        $index = 0;
        $data = array_map(
            function ($item) use (&$index) {
                return array(
                    'index' => ++$index . '.',
                    'fullName' => $this->person($item)
                );
            },
            DBUser::getActiveUsers()
        );
        $this->render(
            'files/View/Member/Clenove/UserList.inc',
            array(
                'data' => $data,
                'uri' => $request->getLiteralURI()
            )
        );
    }
    public function structure($request) {
        $data = DBUser::getUsersWithSkupinaPlatby();
        $skupiny = array();
        $index = 0;
        $currentID = -1;
        $currentKey = 0;

        foreach ($data as $item) {
            if ($item['s_id'] != $currentID) {
                $index = 0;
                $currentID = $item['s_id'];
                $currentKey = count($skupiny);
                $skupiny[$currentKey] = array();
                $skupiny[$currentKey]['info'] = array(
                    'header' => new Tag(
                        'h3',
                        array(),
                        $this->colorbox($item['s_color_rgb'], $item['s_description'])->render(),
                        '&nbsp;&nbsp;' . $item['s_name']
                    )
                );
                $skupiny[$currentKey]['users'] = array();
            }
            $skupiny[$currentKey]['users'][] = array(
                'index' => ++$index . '.',
                'fullName' => $this->person($item),
                'hasPaid' => new Tag(
                    'span',
                    array(
                        'style' => 'font-weight:bold;'
                        . 'color:' . ($item['pi_id'] ? 'green' : 'red')
                    ),
                    $item['pi_id'] ? 'ANO' : 'NE'
                )
            );
        }

        $leftCount = 0;
        $rightCount = 0;
        foreach ($skupiny as &$skupina) {
            $skupina['info']['count'] = count($skupina['users']);
            if ($rightCount >= $leftCount) {
                $skupina['info']['align'] = 'left';
                $leftCount += ($skupina['info']['count']);
            } else {
                $skupina['info']['align'] = 'right';
                $rightCount += ($skupina['info']['count']);
            }
        }

        $this->render(
            'files/View/Member/Clenove/Structure.inc',
            array(
                'data' => $skupiny,
                'uri' => $request->getLiteralURI()
            )
        );
    }
}
