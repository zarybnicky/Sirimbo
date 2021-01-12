<?php
namespace Olymp\Controller\Member;

class Schedule
{
    public static function get()
    {
        \Permissions::checkError('rozpis', P_VIEW);
        \Permissions::checkError('nabidka', P_VIEW);
        $user = \Session::getUser();
        $par = \DBPary::getLatestPartner($user->getId(), $user->getGender());

        $schedules = array_map(
            fn($data) => [
                'type' => 'schedule',
                'id' => $data['r_id'],
                'date' => $data['r_datum'],
                'kde' => $data['r_kde'],
                'fullName' => "{$data['u_jmeno']} {$data['u_prijmeni']}",
                'canEdit' => \Permissions::check('nabidka', P_OWNED, $data['r_trener']),
                'items' => array_map(
                    fn($item) => [
                        'id' => $item['ri_id'],
                        'fullName' => "{$item['u_jmeno']} {$item['u_prijmeni']}",
                        'timeFrom' => $item['ri_od'],
                        'timeTo' => $item['ri_do'],
                        'canReserve' => (
                            $item['ri_partner'] == 0
                            && !$data['r_lock']
                            && !$item['ri_lock']
                            && \Permissions::check('rozpis', P_MEMBER)
                        ),
                        'canCancel' => (
                            $item['ri_partner'] != 0
                            && !$data['r_lock']
                            && !$item['ri_lock']
                            && ((\Permissions::check('rozpis', P_MEMBER) && $par['p_id'] == $item['ri_partner'])
                               || \Permissions::check('rozpis', P_OWNED, $data['r_trener']))
                        )
                    ],
                    \DBRozpis::getRozpisItem($data['r_id'])
                ),
            ],
            array_filter(
                \DBRozpis::getRozpis(),
                fn($item) => $item['r_visible'] && date('Y-m-d') <= $item['r_datum']
            )
        );

        $reservations = array_map(
            function ($data) use ($par) {
                $items = array_map(
                    fn($item) => [
                        'id' => $data['u_id'],
                        'coupleId' => $item['p_id'],
                        'fullName' => "{$item['u_jmeno']} {$item['u_prijmeni']}",
                        'hourCount' => $item['ni_pocet_hod'],
                        'canDelete' =>
                        (!$data['n_lock']
                         && \Permissions::check('nabidka', P_MEMBER)
                         && ($item['p_id'] === $par['p_id']
                            || \Permissions::check('nabidka', P_OWNED, $data['n_trener']))),
                    ],
                    \DBNabidka::getNabidkaItem($data['n_id'])
                );
                return [
                    'type' => 'reservation',
                    'id' => $data['n_id'],
                    'fullName' => "{$data['u_jmeno']} {$data['u_prijmeni']}",
                    'date' => $data['n_od'],
                    'dateEnd' => $data['n_do'],
                    'canEdit' => \Permissions::check('nabidka', P_OWNED, $data['n_trener']),
                    'hourMax' => $data['n_max_pocet_hod'],
                    'hourTotal' => $data['n_pocet_hod'],
                    'hourReserved' => array_reduce($items, fn($c, $x) => $c + $x['hourCount'], 0),
                    'canAdd' => !$data['n_lock'] && \Permissions::check('nabidka', P_MEMBER),
                    'items' => $items
                ];
            },
            array_filter(
                \DBNabidka::getNabidka(),
                fn($item) => $item['n_visible'] && date('Y-m-d') <= $item['n_do']
            ),
        );

        $data = $schedules + $reservations;
        usort($data, function ($a, $b) {
            $c = $a['date'] . $a['id'];
            $d = $b['date'] . $b['id'];
            return ($c > $d ? 1 : ($c < $d ? -1 : 0));
        });
        \Render::twig('Member/Schedule.twig', ['data' => $data]);
    }

    public static function post()
    {
        \Permissions::checkError('rozpis', P_VIEW);
        $user = \Session::getUser();
        $par = \DBPary::getLatestPartner($user->getId(), $user->getGender());

        if (isset($_POST['ri_id'])) {
            $lesson = \DBRozpis::getRozpisItemLesson($_POST['ri_id']);
            $data = \DBRozpis::getSingleRozpis($lesson['ri_id_rodic']);
            $form = static::checkData($data, $_POST['action']);
            if (!$form->isValid()) {
                \Message::warning($form->getMessages());
                return;
            }
            if ($_POST['action'] == 'signup') {
                if (!\Session::getZaplacenoPar()) {
                    \Message::warning('Buď vy nebo váš partner(ka) nemáte zaplacené členské příspěvky');
                } elseif ($lesson['ri_partner']) {
                    \Message::warning('Lekce už je obsazená');
                } else {
                    \DBRozpis::rozpisSignUp($_POST['ri_id'], $par['p_id']);
                }
            } elseif ($_POST['action'] == 'signout') {
                if ($lesson['ri_partner'] == 0) {
                } elseif ($par['p_id'] != $lesson['ri_partner']
                          && !\Permissions::check('rozpis', P_OWNED, $data['r_trener'])
                ) {
                    \Message::warning('Nedostatečná oprávnění!');
                } else {
                    \DBRozpis::rozpisSignOut($_POST['ri_id']);
                }
            }
        } elseif (isset($_POST['n_id'])) {
            $nId = $_POST['n_id'];
            $data = \DBNabidka::getSingleNabidka($nId);
            $form = new \Form();
            $form->checkBool(!$data['n_lock'], 'Tato nabídka je uzamčená', '');
            if ($_POST['hodiny'] ?? null) {
                $form->checkNumeric($_POST['hodiny'], 'Špatný počet hodin', 'hodiny');
            }
            if (!$form->isValid()) {
                \Message::warning($form->getMessages());
                \Redirect::to('/member/treninky');
            }
            if ($_POST['hodiny'] ?? null) {
                if (!\Session::getZaplacenoPar()) {
                    \Message::danger('Buď vy nebo váš partner(ka) nemáte zaplacené členské příspěvky');
                } elseif ($data['n_max_pocet_hod'] > 0 &&
                          (\DBNabidka::getNabidkaLessons($nId, $par['p_id']) + $_POST['hodiny']) > $data['n_max_pocet_hod']
                ) {
                    \Message::danger('Maximální počet hodin na pár je ' . $data['n_max_pocet_hod'] . '!');
                } elseif (($data['n_pocet_hod'] - \DBNabidka::getNabidkaItemLessons($nId)) < $_POST['hodiny']) {
                    \Message::danger('Tolik volných hodin tu není');
                } else {
                    \DBNabidka::addNabidkaItemLessons($par['p_id'], $nId, $_POST['hodiny']);
                }
            } elseif ($_POST['p_id'] ?? null) {
                if (!\DBNabidka::getNabidkaLessons($nId, $_POST['p_id'])) {
                    \Message::danger('Neplatný požadavek!');
                } elseif ($_POST['p_id'] != $par['p_id'] &&
                          !\Permissions::check('nabidka', P_OWNED, $data['n_trener'])
                ) {
                    \Message::danger('Nedostatečná oprávnění!');
                } else {
                    \DBNabidka::removeNabidkaItem($nId, $_POST['p_id']);
                }
            }
        }
        \Redirect::to('/member/treninky');
    }

    protected static function checkData($data, $action = 'signup'): \Form
    {
        $f = new \Form();
        $f->checkBool(!$data['r_lock'], 'Tento rozpis je uzamčený', '');
        $f->checkInArray($action, ['signup', 'signout'], 'Špatná akce', '');
        return $f;
    }
}
