<?php
class Controller_Member_Clenove extends Controller_Abstract
{
    public function __construct($request)
    {
        parent::__construct($request);
        Permissions::checkError('users', P_VIEW);
    }

    public function view($request)
    {
        if (!($id = $request->getId())) {
            $this->redirect('/member/clenove/structure');
        }
        if (!($data = DBUser::getUser($id))) {
            return $this->redirect('/member/clenove/structure');
        }
        new \RenderHelper('files/View/Member/Clenove/Single.inc', [
            'header' => 'Přehled členů',
            'fullName' => $data->getFullName(),
            'email' => $data->getEmail(),
            'telefon' => $data->getPhone(),
            'returnURI' => $request->getReferer(),
            'uri' => $request->getLiteralURI()
        ]);
    }

    public function skupiny($request)
    {
        $currentID = -1;
        $currentKey = 0;
        $data = DBUser::getUsersWithSkupinaPlatby();
        $skupiny = [];
        foreach ($data as $item) {
            if ($item['s_id'] != $currentID) {
                $currentID = $item['s_id'];
                $currentKey = count($skupiny);
                $skupiny[$currentKey] = [
                    'header' => new Tag(
                        'h3',
                        [],
                        new ColorboxHelper($item['s_color_rgb'], $item['s_description'])
                        . '&nbsp;&nbsp;' . $item['s_name']
                    ),
                    'description' => $item['s_description'],
                    'userCount' => 0
                ];
            }
            $skupiny[$currentKey]['userCount']++;
        }
        new \RenderHelper('files/View/Member/Clenove/SkupinyList.inc', [
            'header' => 'Přehled členů',
            'data' => $skupiny,
            'uri' => $request->getLiteralURI()
        ]);
    }

    public function seznam($request)
    {
        $index = 0;
        $data = array_map(
            function ($item) use (&$index) {
                return ['index' => ++$index . '.', 'fullName' => new PersonHelper($item)];
            },
            DBUser::getActiveUsers()
        );
        new \RenderHelper('files/View/Member/Clenove/UserList.inc', [
            'header' => 'Přehled členů',
            'data' => $data,
            'uri' => $request->getLiteralURI()
        ]);
    }

    public function structure($request)
    {
        $data = DBUser::getUsersWithSkupinaPlatby();
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
                        'header' => new Tag(
                            'big',
                            [],
                            new ColorboxHelper($item['s_color_rgb'], $item['s_description'])
                            . '&nbsp;&nbsp;' . $item['s_name']
                        )
                    ],
                    'users' => []
                ];
            }
            $skupiny[$currentKey]['users'][] = [
                'index' => ++$index . '.',
                'fullName' => new PersonHelper($item),
                'hasPaid' => new Tag(
                    'span',
                    ['style' => 'font-weight:bold;color:' . ($item['pi_id'] ? 'green' : 'red')],
                    $item['pi_id'] ? 'ANO' : 'NE'
                )
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

        new \RenderHelper('files/View/Member/Clenove/Structure.inc', [
            'header' => 'Přehled členů',
            'columns' => $columns,
            'uri' => $request->getLiteralURI()
        ]);
    }
}
