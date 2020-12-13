<?php
class Controller_Admin_Platby_Overview extends Controller_Admin_Platby
{
    public function __construct($request)
    {
        parent::__construct($request);
        Permissions::checkError('platby', P_OWNED);
    }

    public function view($request)
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
                            new Colorbox($item['s_color_rgb'], $item['s_description']) .
                            '&nbsp;&nbsp;' . $item['s_name']
                        )
                    ],
                    'users' => []
                ];
            }
            $skupiny[$currentKey]['users'][] = [
                'index' => ++$index . '.',
                'fullName' => $this->person($item)->render(),
                'hasPaid' => new Tag(
                    'span',
                    ['style' => 'font-weight:bold;color:' . ($item['pi_id'] ? 'green' : 'red')],
                    $item['pi_id'] ? 'ANO' : 'NE'
                ),
                'amount' => $item['pi_amount'] == ($item['pc_amount'] * $item['pg_base'])
                ? ('<span style="color:green">' . (int) $item['pi_amount'] . ' Kč</span>')
                : ('<span style="font-weight:bold;color:red">'
                   . (int) $item['pi_amount'] . ' Kč</span> ('
                   . (int) ($item['pc_amount'] * $item['pg_base']) . ' Kč)')
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

        $this->render('files/View/Admin/Platby/Statistics.inc', [
            'header' => 'Správa plateb',
            'subheader' => 'Členové podle skupin',
            'columns' => $columns,
            'uri' => $request->getLiteralURI()
        ]);
    }
}
