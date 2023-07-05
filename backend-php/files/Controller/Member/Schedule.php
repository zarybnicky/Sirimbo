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

        $today = date('Y-m-d');
        $schedules = array_filter(\DBRozpis::getSchedules(), fn($x) => $x['r_visible'] && $today <= $x['r_datum']);
        $reservations = array_filter(\DBNabidka::getNabidka(), fn($x) => $x['n_visible'] && $today <= $x['n_do']);

        $schedules = array_for($schedules, fn($data) => $data + [
            'id' => $data['r_id'],
            'date' => $data['r_datum'],
            'type' => 'schedule',
            'canEdit' => \Permissions::check('nabidka', P_OWNED, $data['r_trener']),
            'items' => array_for(\DBRozpis::getLessons($data['r_id']), fn($item) => $item + [
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
            ]),
        ]);

        $reservations = array_for($reservations, function ($data) use ($par) {
            $items = array_for(\DBNabidka::getReservationItems($data['n_id']), fn($item) => $item + [
                'canDelete' => (
                    !$data['n_lock']
                    && \Permissions::check('nabidka', P_MEMBER)
                    && ($item['p_id'] === $par['p_id']
                       || \Permissions::check('nabidka', P_OWNED, $data['n_trener']))
                ),
            ]);
            return $data + [
                'id' => $data['n_id'],
                'date' => $data['n_od'],
                'type' => 'reservation',
                'canAdd' => !$data['n_lock'] && \Permissions::check('nabidka', P_MEMBER),
                'canEdit' => \Permissions::check('nabidka', P_OWNED, $data['n_trener']),
                'hourReserved' => array_sum(array_column($items, 'ni_pocet_hod')),
                'items' => $items
            ];
        });

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
            $lesson = \DBRozpis::getLesson($_POST['ri_id']);
            $data = \DBRozpis::getSchedule($lesson['ri_id_rodic']);
            $form = static::checkData($data, $_POST['action']);
            if (!$form->isValid()) {
                \Message::warning($form->getMessages());
                return;
            }
            if ($_POST['action'] == 'signup') {
                if ($lesson['ri_partner']) {
                    \Message::warning('Lekce už je obsazená');
                } else {
                    \DBRozpis::reserveLesson($_POST['ri_id'], $par['p_id']);
                }
            } elseif ($_POST['action'] == 'signout') {
                if ($lesson['ri_partner'] === null) {
                } elseif ($par['p_id'] != $lesson['ri_partner']
                          && !\Permissions::check('rozpis', P_OWNED, $data['r_trener'])
                ) {
                    \Message::warning('Nedostatečná oprávnění!');
                } else {
                    \DBRozpis::cancelLesson($_POST['ri_id']);
                }
            }
        } elseif (isset($_POST['n_id'])) {
            $nId = $_POST['n_id'];
            $data = \DBNabidka::getSingleNabidka($nId);
            $form = new \Form();
            $form->checkBool(!$data['n_lock'], 'Tato nabídka je uzamčená');
            if ($_POST['hodiny'] ?? null) {
                $form->checkNumeric($_POST['hodiny'], 'Špatný počet hodin');
            }
            if (!$form->isValid()) {
                \Message::warning($form->getMessages());
                \Redirect::to('/member/treninky');
            }
            if ($_POST['hodiny'] ?? null) {
                if ($data['n_max_pocet_hod'] > 0 &&
                          (\DBNabidka::getNabidkaLessons($nId, $par['p_id']) + $_POST['hodiny']) > $data['n_max_pocet_hod']
                ) {
                    \Message::danger('Maximální počet hodin na pár je ' . $data['n_max_pocet_hod'] . '!');
                } elseif (($data['n_pocet_hod'] - \DBNabidka::getReservationLessons($nId)) < $_POST['hodiny']) {
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
        $f->checkBool(!$data['r_lock'], 'Tento rozpis je uzamčený');
        $f->checkInArray($action, ['signup', 'signout'], 'Špatná akce');
        return $f;
    }
}
