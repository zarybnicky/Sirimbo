<?php
namespace Olymp\Controller\Member;

class Rozpis
{
    public static function get()
    {
        \Permissions::checkError('rozpis', P_VIEW);
        $user = \Session::getUser();
        $par = \DBPary::getLatestPartner($user->getId(), $user->getGender());

        $data = array_map(
            fn($rozpis) => [
                'id' => $rozpis['r_id'],
                'datum' => \Format::date($rozpis['r_datum']),
                'kde' => $rozpis['r_kde'],
                'fullName' => $rozpis['u_jmeno'] . ' ' . $rozpis['u_prijmeni'],
                'canEdit' => \Permissions::check('nabidka', P_OWNED, $rozpis['r_trener']),
                'items' => array_map(
                    fn($item) => [
                        'id' => $item['ri_id'],
                        'fullName' => "{$item['u_jmeno']} {$item['u_prijmeni']}",
                        'timeFrom' => \Format::time($item['ri_od'], 1),
                        'timeTo' => \Format::time($item['ri_do'], 1),
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
                            && ((\Permissions::check('rozpis', P_MEMBER) && $par['p_id'] == $item['ri_partner'])
                               || \Permissions::check('rozpis', P_OWNED, $rozpis['r_trener']))
                        )
                    ],
                    \DBRozpis::getRozpisItem($rozpis['r_id'])
                ),
            ],
            array_filter(
                \DBRozpis::getRozpis(),
                fn($item) => $item['r_visible'] && (date('Y-m-d') <= $item['r_datum'])
            )
        );

        if ($data) {
            \Render::page('files/View/Member/Rozpis/Overview.inc', [
                'header' => 'Rozpis tréninků',
                'data' => $data
            ]);
        } else {
            \Render::twig('Empty.twig', [
                'header' => 'Rozpis tréninků',
                'notice' => 'Žádné aktuální rozpisy nejsou k dispozici.'
            ]);
        }
    }

    public static function post()
    {
        \Permissions::checkError('rozpis', P_VIEW);
        $user = \Session::getUser();
        $par = \DBPary::getLatestPartner($user->getId(), $user->getGender());

        $data = \DBRozpis::getSingleRozpis($_POST['ri_id']);
        $lesson = \DBRozpis::getRozpisItemLesson($_POST['ri_id']);
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
                      && !\Permissions::check('rozpis', P_OWNED, $data['n_trener'])
            ) {
                \Message::warning('Nedostatečná oprávnění!');
            } else {
                \DBRozpis::rozpisSignOut($_POST['ri_id']);
            }
        }
        \Redirect::to('/member/rozpis');
    }

    protected static function checkData($data, $action = 'signup'): \Form
    {
        $f = new \Form();
        $f->checkBool(!$data['r_lock'], 'Tento rozpis je uzamčený', '');
        $f->checkInArray($action, ['signup', 'signout'], 'Špatná akce', '');
        return $f;
    }
}
