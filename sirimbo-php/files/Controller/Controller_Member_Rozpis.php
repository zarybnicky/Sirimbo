<?php
class Controller_Member_Rozpis
{
    public function view($request)
    {
        \Permissions::checkError('rozpis', P_VIEW);
        if ($request->post()) {
            $this->processPost($request);
            new \RedirectHelper('/member/rozpis');
        }

        $data = array_map(
            function ($rozpis) {
                $items = array_map(
                    function ($item) use ($rozpis) {
                        return [
                            'id' => $item['ri_id'],
                            'fullName' => "{$item['u_jmeno']} {$item['u_prijmeni']}",
                            'timeFrom' => formatTime($item['ri_od'], 1),
                            'timeTo' => formatTime($item['ri_do'], 1),
                            'canReserve' => (
                                $item['ri_partner'] == 0
                                && !$rozpis['r_lock']
                                && !$item['ri_lock']
                                && \Permissions::check('rozpis', P_MEMBER)
                            ),
                            'canCancel' => (
                                $item['ri_partner'] != 0
                                && !$rozpis['r_lock']
                                && !$item['ri_lock']
                                && ((\Permissions::check('rozpis', P_MEMBER)
                                     && Session::getParID() == $item['ri_partner'])
                                    || \Permissions::check('rozpis', P_OWNED, $rozpis['r_trener']))
                            )
                        ];
                    },
                    \DBRozpis::getRozpisItem($rozpis['r_id'])
                );

                return [
                    'id' => $rozpis['r_id'],
                    'datum' => formatDate($rozpis['r_datum']),
                    'kde' => $rozpis['r_kde'],
                    'fullName' => $rozpis['u_jmeno'] . ' ' . $rozpis['u_prijmeni'],
                    'items' => $items,
                    'canEdit' => \Permissions::check('nabidka', P_OWNED, $rozpis['r_trener'])
                ];
            },
            array_filter(
                \DBRozpis::getRozpis(),
                function ($item) {
                    return $item['r_visible'] && (date('Y-m-d') <= $item['r_datum']);
                }
            )
        );

        if ($data) {
            new \RenderHelper('files/View/Member/Rozpis/Overview.inc', [
                'header' => 'Rozpis tréninků',
                'data' => $data
            ]);
        } else {
            new \RenderHelper('files/View/Empty.inc', [
                'header' => 'Rozpis tréninků',
                'notice' => 'Žádné aktuální rozpisy nejsou k dispozici.'
            ]);
        }
    }

    protected function checkData($request, $data, $action = 'signup'): \Form
    {
        $f = new \Form();
        $f->checkBool(!$data['r_lock'], 'Tento rozpis je uzamčený', '');
        $f->checkInArray($action, ['signup', 'signout'], 'Špatná akce', '');
        return $f;
    }

    protected function processPost($request)
    {
        $data = \DBRozpis::getSingleRozpis($request->post('ri_id'));
        $lesson = \DBRozpis::getRozpisItemLesson($request->post('ri_id'));
        $form = $this->checkData($request, $data, $request->post('action'));
        if (!$form->isValid()) {
            return new \MessageHelper('warning', $form->getMessages());
        }
        if ($request->post('action') == 'signup') {
            if (!Session::getZaplacenoPar()) {
                new \MessageHelper('warning', 'Buď vy nebo váš partner(ka) nemáte zaplacené členské příspěvky');
            } elseif ($lesson['ri_partner']) {
                new \MessageHelper('warning', 'Lekce už je obsazená');
            } else {
                \DBRozpis::rozpisSignUp($request->post('ri_id'), Session::getParID());
            }
        } elseif ($request->post('action') == 'signout') {
            if ($lesson['ri_partner'] == 0) {
            } elseif (Session::getParID() != $lesson['ri_partner']
                      && !\Permissions::check('rozpis', P_OWNED, $data['n_trener'])
            ) {
                new \MessageHelper('warning', 'Nedostatečná oprávnění!');
            } else {
                \DBRozpis::rozpisSignOut($request->post('ri_id'));
            }
        }
    }
}
