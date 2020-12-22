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

    public static function body()
    {
        $user = \Session::getUser();
        $par = \DBPary::getLatestPartner($user->getId(), $user->getGender());
        return new \RenderHelper('files/View/Member/Profil/CoupleData.inc', [
            'header' => 'Změna třídy a bodů',
            'stt_trida' => $par['p_stt_trida'],
            'stt_body' => $par['p_stt_body'],
            'stt_finale' => $par['p_stt_finale'],
            'lat_trida' => $par['p_lat_trida'],
            'lat_body' => $par['p_lat_body'],
            'lat_finale' => $par['p_lat_finale']
        ]);
    }

    public static function bodyPost()
    {
        $user = \Session::getUser();
        $par = \DBPary::getLatestPartner($user->getId(), $user->getGender());

        $form = static::checkData();
        if (!$form->isValid()) {
            \Message::warning($form->getMessages());
            return new \RenderHelper('files/View/Member/Profil/CoupleData.inc', [
                'header' => 'Změna třídy a bodů',
                'stt_trida' => $_POST['stt_trida'] ?? '',
                'stt_body' => $_POST['stt_body'] ?? '',
                'stt_finale' => $_POST['stt_finale'] ?? '',
                'lat_trida' => $_POST['lat_trida'] ?? '',
                'lat_body' => $_POST['lat_body'] ?? '',
                'lat_finale' => $_POST['lat_finale'] ?? ''
            ]);
        }
        $stt_amend = constant('self::AMEND_' . $_POST['stt-trida']);
        $stt_bonus = constant('self::BONUS_' . $_POST['stt-trida']);
        $lat_amend = constant('self::AMEND_' . $_POST['lat-trida']);
        $lat_bonus = constant('self::BONUS_' . $_POST['lat-trida']);

        $stt_body_capped = $_POST['stt-body'] > 200 ? 200 : $_POST['stt-body'];
        $lat_body_capped = $_POST['lat-body'] > 200 ? 200 : $_POST['lat-body'];

        $hodnoceni
            = ($stt_body_capped + 40 * $_POST['stt-finale']) * $stt_amend
            + ($lat_body_capped + 40 * $_POST['lat-finale']) * $lat_amend
            + $stt_bonus
            + $lat_bonus;

        \DBPary::editTridaBody(
            $par['p_id'],
            $_POST['stt-trida'],
            $_POST['stt-body'],
            $_POST['stt-finale'],
            $_POST['lat-trida'],
            $_POST['lat-body'],
            $_POST['lat-finale'],
            $hodnoceni
        );
        \Redirect::to("/member/profil");
    }

    public static function partner()
    {
        $user = \Session::getUser();
        $couple = \DBPary::getLatestPartner($user->getId(), $user->getGender());
        $havePartner = !empty($couple) && $couple['u_id'];
        $_POST['partner'] = $havePartner ? $couple['u_id'] : '0';
        new \RenderHelper('files/View/Member/Profil/PartnerOverview.inc', [
            'header' => 'Profil',
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

    public static function pary()
    {
        \Permissions::checkError('pary', P_VIEW);
        $data = \DBPary::getActiveParyByHodnoceni();
        if (empty($data)) {
            return new \RenderHelper('files/View/Empty.inc', [
                'header' => 'Žebříček párů',
                'notice' => 'Žádné páry nejsou v databázi'
            ]);
        }

        new \RenderHelper('files/View/Member/Pary/Overview.inc', [
            'header' => 'Žebříček párů',
            'data' => array_map(
                fn($item) => [
                    'id' => $item['p_id'],
                    'partnerName' => $item['guy_name'] . ' ' . $item['guy_surname'],
                    'partnerkaName' => $item['gal_name'] . ' ' . $item['gal_surname'],
                    'latina' => $item['p_stt_trida'] . ' ' . $item['p_stt_body'] . 'F' . $item['p_stt_finale'],
                    'standard' => $item['p_lat_trida'] . ' ' . $item['p_lat_body'] . 'F' . $item['p_lat_finale'],
                    'hodnoceni' => $item['p_hodnoceni']
                ],
                $data
            ),
        ]);
    }

    private static function checkData()
    {
        $f = new \Form();
        $f->checkInArray(
            $_POST['stt-trida'],
            ['Z', 'H', 'D', 'C', 'B', 'A', 'M'],
            'Neplatná standardní třída',
            'stt-trida'
        );
        $f->checkInArray(
            $_POST['lat-trida'],
            ['Z', 'H', 'D', 'C', 'B', 'A', 'M'],
            'Neplatná latinská třída',
            'lat-trida'
        );
        $f->checkNumberBetween(
            $_POST['stt-body'],
            0,
            1000,
            'Špatný počet standardních bodů',
            'stt-body'
        );
        $f->checkNumberBetween(
            $_POST['lat-body'],
            0,
            1000,
            'Špatný počet latinských bodů',
            'lat-body'
        );
        $f->checkNumberBetween(
            $_POST['stt-finale'],
            0,
            10,
            'Špatný počet standardních finálí',
            'stt-finale'
        );
        $f->checkNumberBetween(
            $_POST['lat-finale'],
            0,
            10,
            'Špatný počet latinských finálí',
            'lat-finale'
        );
        return $f;
    }
}
