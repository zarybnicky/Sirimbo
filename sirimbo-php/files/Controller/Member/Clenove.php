<?php
namespace Olymp\Controller\Member;

class Clenove
{
    public static function single($id)
    {
        \Permissions::checkError('users', P_VIEW);
        if (!($data = \DBUser::getUser($id))) {
            \Redirect::to('/member/clenove');
        }
        \Render::page('files/View/Member/Clenove/Single.inc', [
            'header' => 'Přehled členů',
            'fullName' => $data->getFullName(),
            'email' => $data->getEmail(),
            'telefon' => $data->getPhone(),
            'returnURI' => $_SERVER['HTTP_REFERER'],
        ]);
    }

    public static function groups()
    {
        \Permissions::checkError('users', P_VIEW);
        $currentID = -1;
        $currentKey = 0;
        $data = \DBUser::getUsersWithSkupinaPlatby();
        $skupiny = [];
        foreach ($data as $item) {
            if ($item['s_id'] != $currentID) {
                $currentID = $item['s_id'];
                $currentKey = count($skupiny);
                $skupiny[$currentKey] = [
                    'header' => "<h3>"
                    . new \ColorboxHelper($item['s_color_rgb'], $item['s_description'])
                    . '&nbsp;&nbsp;' . $item['s_name'] . "</h3>",
                    'description' => $item['s_description'],
                    'userCount' => 0
                ];
            }
            $skupiny[$currentKey]['userCount']++;
        }
        \Render::page('files/View/Member/Clenove/SkupinyList.inc', [
            'header' => 'Přehled členů',
            'data' => $skupiny,
        ]);
    }

    public static function list()
    {
        \Permissions::checkError('users', P_VIEW);
        $index = 0;
        $data = array_map(
            function ($item) use (&$index) {
                return ['index' => ++$index . '.', 'fullName' => new \PersonHelper($item)];
            },
            \DBUser::getActiveUsers()
        );
        \Render::page('files/View/Member/Clenove/UserList.inc', [
            'header' => 'Přehled členů',
            'data' => $data,
        ]);
    }

    public static function structure()
    {
        \Permissions::checkError('users', P_VIEW);
        $data = \DBUser::getUsersWithSkupinaPlatby();
        $skupiny = [];
        $index = 0;
        $currentID = -1;
        $currentKey = 0;

        foreach ($data as $item) {
            if ($item['s_id'] != $currentID) {
                $index = 0;
                $currentID = $item['s_id'];
                $currentKey = count($skupiny);
                $skupiny[$currentKey] = [
                    'info' => [
                        'header' => "<big>"
                        .  new \ColorboxHelper($item['s_color_rgb'], $item['s_description'])
                        . '&nbsp;&nbsp;' . $item['s_name'] . "</big>"
                    ],
                    'users' => []
                ];
            }
            $skupiny[$currentKey]['users'][] = [
                'index' => ++$index . '.',
                'fullName' => new \PersonHelper($item),
                'hasPaid' => '<span style="font-weight:bold;color:"'
                . ($item['pi_id'] ? 'green' : 'red') . '">'
                . ($item['pi_id'] ? 'ANO' : 'NE') . "</span>"
            ];
        }

        $columns = [[], []];
        $leftCount = 0;
        $rightCount = 0;
        foreach ($skupiny as &$skupina) {
            $skupina['info']['count'] = count($skupina['users']);
            if ($rightCount >= $leftCount) {
                $columns[0][] = $skupina;
                $leftCount += ($skupina['info']['count']);
            } else {
                $columns[1][] = $skupina;
                $rightCount += ($skupina['info']['count']);
            }
        }

        \Render::page('files/View/Member/Clenove/Structure.inc', [
            'header' => 'Přehled členů',
            'columns' => $columns,
        ]);
    }
}
