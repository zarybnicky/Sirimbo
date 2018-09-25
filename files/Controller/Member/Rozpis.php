<?php
require_once 'files/Controller/Member.php';
class Controller_Member_Rozpis extends Controller_Member
{
    public function __construct()
    {
        Permissions::checkError('rozpis', P_VIEW);
    }

    public function view($request)
    {
        if ($request->post()) {
            $this->redirect()->setMessage($this->processPost($request));
            $this->redirect('/member/rozpis');
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
                                && Permissions::check('rozpis', P_MEMBER)
                            ),
                            'canCancel' => (
                                $item['ri_partner'] != 0
                                && !$rozpis['r_lock']
                                && !$item['ri_lock']
                                && ((Permissions::check('rozpis', P_MEMBER)
                                     && User::getParID() == $item['ri_partner'])
                                    || Permissions::check('rozpis', P_OWNED, $rozpis['r_trener']))
                            )
                        ];
                    },
                    DBRozpis::getRozpisItem($rozpis['r_id'])
                );

                return [
                    'id' => $rozpis['r_id'],
                    'datum' => formatDate($rozpis['r_datum']),
                    'kde' => $rozpis['r_kde'],
                    'fullName' => $rozpis['u_jmeno'] . ' ' . $rozpis['u_prijmeni'],
                    'items' => $items,
                    'canEdit' => Permissions::check('nabidka', P_OWNED, $rozpis['r_trener'])
                ];
            },
            array_filter(
                DBRozpis::getRozpis(),
                function ($item) {
                    return $item['r_visible'] && (date('Y-m-d') <= $item['r_datum']);
                }
            )
        );

        if ($data) {
            $this->render(
                'files/View/Member/Rozpis/Overview.inc',
                ['data' => $data]
            );
        } else {
            $this->render(
                'files/View/Empty.inc',
                [
                    'nadpis' => 'Rozpis tréninků',
                    'notice' => 'Žádné aktuální rozpisy nejsou k dispozici.'
                ]
            );
        }
    }

    protected function checkData($request, $data, $action = 'signup')
    {
        $f = new Form();
        $f->checkBool(!$data['r_lock'], 'Tento rozpis je uzamčený', '');
        $f->checkInArray($action, ['signup', 'signout'], 'Špatná akce', '');

        return $f->isValid() ? true : $f;
    }

    protected function processPost($request)
    {
        $data = DBRozpis::getSingleRozpis($request->post('ri_id'));
        $lesson = DBRozpis::getRozpisItemLesson($request->post('ri_id'));

        if (is_object($f = $this->checkData($request, $data, $request->post('action')))) {
            return $f->getMessages();
        }
        if ($request->post('action') == 'signup') {
            if (!User::getZaplaceno() || (User::getPartnerID() > 0 && !User::getZaplaceno(true))) {
                return 'Buď vy nebo váš partner(ka) nemáte zaplacené členské příspěvky';
            } elseif ($lesson['ri_partner']) {
                return 'Už je obsazeno';
            } else {
                DBRozpis::rozpisSignUp($request->post('ri_id'), User::getParID());
                return 'Hodina přidána';
            }
        } elseif ($request->post('action') == 'signout') {
            if ($lesson['ri_partner'] == 0) {
                return 'Už je prázdno';
            } elseif (User::getParID() != $lesson['ri_partner']
                      && !Permissions::check('rozpis', P_OWNED, $data['n_trener'])
            ) {
                return 'Nedostatečná oprávnění!';
            } else {
                DBRozpis::rozpisSignOut($request->post('ri_id'));
                return 'Hodina odebrána';
            }
        }
    }
}
