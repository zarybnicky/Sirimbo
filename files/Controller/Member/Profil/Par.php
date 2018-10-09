<?php
require_once 'files/Controller/Member/Profil.php';
class Controller_Member_Profil_Par extends Controller_Member_Profil
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

    public function view($request)
    {
        $latest = DBPary::getLatestPartner(
            User::getUserID(),
            User::getUserPohlavi()
        );
        $this->render('files/View/Member/Profil/CoupleOverview.inc', [
            'header' => 'Profil',
            'havePartner' => !empty($latest) && $latest['u_id'],
            'partnerFullName' => $latest['u_jmeno'] . ' ' . $latest['u_prijmeni'],
            'sttTrida' => $latest['p_stt_trida'],
            'sttBody' => $latest['p_stt_body'],
            'sttFinale' => $latest['p_stt_finale'],
            'latTrida' => $latest['p_lat_trida'],
            'latBody' => $latest['p_lat_body'],
            'latFinale' => $latest['p_lat_finale'],
            'hodnoceni' => $latest['p_hodnoceni']
        ]);
    }

    public function body($request)
    {
        if (!$request->post() || is_object($f = $this->checkData($request))) {
            if (!$request->post()) {
                $par = DBPary::getSinglePar(User::getParID());
                $request->post('stt-trida', $par['p_stt_trida']);
                $request->post('stt-body', $par['p_stt_body']);
                $request->post('stt-finale', $par['p_stt_finale']);
                $request->post('lat-trida', $par['p_lat_trida']);
                $request->post('lat-body', $par['p_lat_body']);
                $request->post('lat-finale', $par['p_lat_finale']);
            } else {
                $this->redirect()->setMessage($f->getMessages());
            }
            $this->render('files/View/Member/Profil/CoupleData.inc', [
                'header' => 'Změna třídy a bodů',
                'stt_trida' => $request->post('stt_trida') ?: '',
                'stt_body' => $request->post('stt_body') ?: '',
                'stt_finale' => $request->post('stt_finale') ?: '',
                'lat_trida' => $request->post('lat_trida') ?: '',
                'lat_body' => $request->post('lat_body') ?: '',
                'lat_finale' => $request->post('lat_finale') ?: ''
            ]);
            return;
        }
        $stt_amend = constant('self::AMEND_' . $request->post('stt-trida'));
        $stt_bonus = constant('self::BONUS_' . $request->post('stt-trida'));
        $lat_amend = constant('self::AMEND_' . $request->post('lat-trida'));
        $lat_bonus = constant('self::BONUS_' . $request->post('lat-trida'));

        $stt_body_capped = $request->post('stt-body') > 200 ? 200 : $request->post('stt-body');
        $lat_body_capped = $request->post('lat-body') > 200 ? 200 : $request->post('lat-body');

        $hodnoceni
            = ($stt_body_capped + 40 * $request->post('stt-finale')) * $stt_amend
            + ($lat_body_capped + 40 * $request->post('lat-finale')) * $lat_amend
            + $stt_bonus
            + $lat_bonus;

        DBPary::editTridaBody(
            User::getParID(),
            $request->post('stt-trida'),
            $request->post('stt-body'),
            $request->post('stt-finale'),
            $request->post('lat-trida'),
            $request->post('lat-body'),
            $request->post('lat-finale'),
            $hodnoceni
        );
        $this->redirect("/member/profil/par");
    }

    public function partner($request)
    {
        $latest = DBPary::getLatestPartner(
            User::getUserID(),
            User::getUserPohlavi()
        );
        $havePartner = !empty($latest) && $latest['u_id'];

        if ($request->post()) {
            if (!$request->post("partner") ||
                ($request->post('action') == 'dumpthem' && $havePartner)
            ) {
                DBPary::noPartner(User::getUserID());
                DBPary::noPartner($latest['u_id']);
                $this->redirect('/member/profil/par');
            }
            if ($request->post('partner') == $latest['u_id'] ||
                (!$request->post('partner') && $latest['u_id'] == '0')
            ) {
                $this->redirect('/member/profil/par');
            }
            if (User::getUserPohlavi() == "m") {
                DBPary::newPartnerRequest(
                    User::getUserID(),
                    User::getUserID(),
                    $request->post("partner")
                );
            } else {
                DBPary::newPartnerRequest(
                    User::getUserID(),
                    $request->post("partner"),
                    User::getUserID()
                );
            }
            $this->redirect()->info('Žádost o partnerství odeslána');
            $this->redirect('/member/profil/par');
        }

        $request->post('partner', $havePartner ? $latest['u_id'] : '0');
        $this->render('files/View/Member/Profil/PartnerOverview.inc', [
            'header' => 'Profil',
            'havePartner' => $havePartner,
            'partnerID' => $latest['u_id'],
            'partnerFullName' => $latest['u_jmeno'] . ' ' . $latest['u_prijmeni'],
            'users' => (User::getUserPohlavi() == "m")
                        ? DBUser::getUsersByPohlavi("f")
                        : DBUser::getUsersByPohlavi("m")
        ]);
    }

    public function zadost($request)
    {
        if (!$request->post('action')) {
            $this->redirect('/member/profil');
        }
        switch ($request->post('action')) {
            case 'accept':
            case 'refuse':
                $requests = DBPary::getPartnerRequestsForMe(User::getUserID());
                foreach ($requests as $req) {
                    if ($req['pn_id'] != $request->post('id')) {
                        continue;
                    }

                    if ($request->post('action') == 'accept') {
                        DBPary::acceptPartnerRequest($request->post('id'));
                        $this->redirect()->success('žádost přijata');
                    } else {
                        DBPary::deletePartnerRequest($request->post('id'));
                        $this->redirect()->info('žádost zamitnuta');
                    }
                    $this->redirect('/member/profil/par');
                }
                break;

            case 'cancel':
                $requests = DBPary::getPartnerRequestsByMe(User::getUserID());
                foreach ($requests as $req) {
                    if ($req['pn_id'] != $request->post('id')) {
                        continue;
                    }
                    DBPary::deletePartnerRequest($request->post('id'));
                    $this->redirect()->info('žádost zrušena');
                    $this->redirect('/member/profil/par');
                }
                break;

            default:
                $this->redirect('/member/profil');
                break;
        }
        $this->redirect()->warning('Žádná taková žádost tu není');
        $this->redirect('/member/profil');
    }

    private function checkData($request)
    {
        $f = new Form();
        $f->checkInArray(
            $request->post('stt-trida'),
            ['Z', 'H', 'D', 'C', 'B', 'A', 'M'],
            'Neplatná standardní třída',
            'stt-trida'
        );
        $f->checkInArray(
            $request->post('lat-trida'),
            ['Z', 'H', 'D', 'C', 'B', 'A', 'M'],
            'Neplatná latinská třída',
            'lat-trida'
        );
        $f->checkNumberBetween(
            $request->post('stt-body'),
            0,
            1000,
            'Špatný počet standardních bodů',
            'stt-body'
        );
        $f->checkNumberBetween(
            $request->post('lat-body'),
            0,
            1000,
            'Špatný počet latinských bodů',
            'lat-body'
        );
        $f->checkNumberBetween(
            $request->post('stt-finale'),
            0,
            10,
            'Špatný počet standardních finálí',
            'stt-finale'
        );
        $f->checkNumberBetween(
            $request->post('lat-finale'),
            0,
            10,
            'Špatný počet latinských finálí',
            'lat-finale'
        );
        return $f->isValid() ? null : $f;
    }
}
