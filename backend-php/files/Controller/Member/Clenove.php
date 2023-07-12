<?php
namespace Olymp\Controller\Member;

class Clenove
{
    public static function single($id)
    {
        \Permissions::checkError('users', P_VIEW);
        if (!($data = \DBUser::getUserData($id))) {
            \Redirect::to('/member/clenove');
        }
        \Render::twig('Member/ClenoveSingle.twig', [
            'user' => \User::fromArray($data),
            'returnURI' => $_SERVER['HTTP_REFERER'],
        ]);
    }

    public static function list()
    {
        \Permissions::checkError('users', P_VIEW);
        \Render::twig('Member/ClenoveUserList.twig', [
            'data' => \Database::queryArray("SELECT users.*,skupiny.* FROM users LEFT JOIN skupiny ON users.u_skupina=skupiny.s_id WHERE u_system='0' AND u_confirmed='1' AND u_ban='0' ORDER BY u_prijmeni"),
        ]);
    }

    public static function structure()
    {
        \Permissions::checkError('users', P_VIEW);
        $skupiny = [];
        $currentID = -1;
        $currentKey = 0;
        foreach (\DBUser::getUsersWithSkupinaPlatby() as $item) {
            if ($item['s_id'] != $currentID) {
                $currentID = $item['s_id'];
                $currentKey = count($skupiny);
                $skupiny[$currentKey] = [
                    'color' => $item['s_color_rgb'],
                    'name' => $item['s_name'],
                    'id' => $item['s_id'],
                    'users' => [],
                    'count' => 0,
                ];
            }
            $skupiny[$currentKey]['users'][] = $item;
            $skupiny[$currentKey]['count']++;
        }
        usort(
            $skupiny,
            function ($a, $b) {
                $a1 = $a['count'];
                $b1 = $b['count'];
                return $a1 < $b1 ? 1 : ($a1 > $b1 ? -1 : 0);
            }
        );

        $columns = [[], []];
        $leftCount = 0;
        $rightCount = 0;
        foreach ($skupiny as &$skupina) {
            if ($rightCount >= $leftCount) {
                $columns[0][] = $skupina;
                $leftCount += $skupina['count'];
            } else {
                $columns[1][] = $skupina;
                $rightCount += $skupina['count'];
            }
        }
        \Render::twig('Member/ClenoveStructure.twig', [
            'columns' => $columns
        ]);
    }
}
