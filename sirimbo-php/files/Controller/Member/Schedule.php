<?php
namespace Olymp\Controller\Member;

class Schedule
{
    public static function post()
    {
        \Permissions::checkError('rozpis', P_VIEW);
        $user = \Session::getUser();
        $par = \DBPary::getLatestPartner($user->getId(), $user->getGender());

        if (isset($_POST['ri_id'])) {
            $lesson = \DBRozpis::getLesson($_POST['ri_id']);
            $data = \DBRozpis::getSchedule($lesson['ri_id_rodic']);
            if ($_POST['action'] == 'signup') {
                if (!\Session::getZaplacenoPar()) {
                    \Message::warning('Buď vy nebo váš partner(ka) nemáte zaplacené členské příspěvky');
                } elseif ($lesson['ri_partner']) {
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
                if (!\Session::getZaplacenoPar()) {
                    \Message::danger('Buď vy nebo váš partner(ka) nemáte zaplacené členské příspěvky');
                } elseif ($data['n_max_pocet_hod'] > 0 &&
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
}
