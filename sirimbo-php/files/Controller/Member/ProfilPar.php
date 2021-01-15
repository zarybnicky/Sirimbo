<?php
namespace Olymp\Controller\Member;

class ProfilPar
{
    const AMEND_Z = 0.2;
    const AMEND_H = 0.5;
    const AMEND_D = 1.0;
    const AMEND_C = 1.6;
    const AMEND_B = 2.1;
    const AMEND_A = 2.7;
    const AMEND_M = 3.4;

    const BONUS_Z = 0;
    const BONUS_H = 80;   //400*AMEND_Z + BONUS_Z
    const BONUS_D = 280;  //400*AMEND_H + BONUS_H
    const BONUS_C = 680;  //400*AMEND_D + BONUS_D
    const BONUS_B = 1320; //400*AMEND_C + BONUS_C
    const BONUS_A = 2160; //400*AMEND_B + BONUS_B
    const BONUS_M = 3240; //400*AMEND_A + BONUS_A

    public static function partner()
    {
        $user = \Session::getUser();
        $couple = \DBPary::getLatestPartner($user->getId(), $user->getGender());
        $havePartner = !empty($couple) && $couple['u_id'];
        $_POST['partner'] = $havePartner ? $couple['u_id'] : '0';
        \Render::twig('Member/ProfilPartner.twig', [
            'havePartner' => $havePartner,
            'partnerID' => $couple['u_id'],
            'partnerFullName' => $couple['u_jmeno'] . ' ' . $couple['u_prijmeni'],
            'users' => \DBUser::getUsersByPohlavi(($user->getGender() == "m") ? "f" : "m")
        ]);
    }

    public static function partnerPost()
    {
        $user = \Session::getUser();
        $couple = \DBPary::getLatestPartner($user->getId(), $user->getGender());
        $havePartner = !empty($couple) && $couple['u_id'];
        if (!isset($_POST["partner"]) || ($_POST['action'] == 'dumpthem' && $havePartner)) {
            \DBPary::noPartner($user->getId());
            \DBPary::noPartner($couple['u_id']);
            \Redirect::to('/member/profil');
        }
        if ($_POST['partner'] == $couple['u_id'] || (!$_POST['partner'] && $couple['u_id'] == '0')) {
            \Redirect::to('/member/profil');
        }
        if ($user->getGender() == "m") {
            \DBPary::newPartnerRequest($user->getId(), $user->getId(), $_POST["partner"]);
        } else {
            \DBPary::newPartnerRequest($user->getId(), $_POST["partner"], $user->getId());
        }
        \Message::info('Žádost o partnerství odeslána');
        \Redirect::to('/member/profil');
    }

    public static function zadost()
    {
        switch ($_POST['action']) {
            case 'accept':
            case 'refuse':
                $requests = \DBPary::getPartnerRequestsForMe(\Session::getUser()->getId());
                foreach ($requests as $req) {
                    if ($req['pn_id'] != $_POST['id']) {
                        continue;
                    }

                    if ($_POST['action'] == 'accept') {
                        \DBPary::acceptPartnerRequest($_POST['id']);
                        \Message::success('žádost přijata');
                    } else {
                        \DBPary::deletePartnerRequest($_POST['id']);
                        \Message::info('žádost zamitnuta');
                    }
                    \Redirect::to('/member/profil');
                }
                break;

            case 'cancel':
                $requests = \DBPary::getPartnerRequestsByMe(\Session::getUser()->getId());
                foreach ($requests as $req) {
                    if ($req['pn_id'] != $_POST['id']) {
                        continue;
                    }
                    \DBPary::deletePartnerRequest($_POST['id']);
                    \Message::info('žádost zrušena');
                    \Redirect::to('/member/profil');
                }
                break;

            default:
                \Redirect::to('/member/profil');
        }
        \Message::warning('Žádná taková žádost tu není');
        \Redirect::to('/member/profil');
    }

    private static function checkData()
    {
        $classes = ['Z', 'H', 'D', 'C', 'B', 'A', 'M'];
        $f = new \Form();
        $f->checkInArray($_POST['stt-trida'], $classes, 'Neplatná standardní třída');
        $f->checkInArray($_POST['lat-trida'], $classes, 'Neplatná latinská třída');
        $f->checkNumberBetween($_POST['stt-body'], 0, 1000, 'Špatný počet standardních bodů');
        $f->checkNumberBetween($_POST['lat-body'], 0, 1000, 'Špatný počet latinských bodů');
        $f->checkNumberBetween($_POST['stt-finale'], 0, 10, 'Špatný počet standardních finálí');
        $f->checkNumberBetween($_POST['lat-finale'], 0, 10, 'Špatný počet latinských finálí');
        return $f;
    }
}
