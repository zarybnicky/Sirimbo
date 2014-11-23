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

    public function view($id = null) {
        $latest = DBPary::getLatestPartner(User::getUserID(), User::getUserPohlavi());
        $this->render(
            'files/View/Member/Profil/CoupleOverview.inc',
            array(
                'havePartner' => !empty($latest) && $latest['u_id'],
                'partnerFullName' => $latest['u_jmeno'] . ' ' . $latest['u_prijmeni'],
                'sttTrida' => $latest['p_stt_trida'],
                'sttBody' => $latest['p_stt_body'],
                'sttFinale' => $latest['p_stt_finale'],
                'latTrida' => $latest['p_lat_trida'],
                'latBody' => $latest['p_lat_body'],
                'latFinale' => $latest['p_lat_finale'],
                'hodnoceni' => $latest['p_hodnoceni']
            )
        );
    }
    public function body($id = null) {
        if (empty($_POST) || is_object($f = $this->_checkData())) {
            if (empty($_POST)) {
                $par = DBPary::getSinglePar(User::getParID());
                post('stt-trida', $par['p_stt_trida']);
                post('stt-body', $par['p_stt_body']);
                post('stt-finale', $par['p_stt_finale']);
                post('lat-trida', $par['p_lat_trida']);
                post('lat-body', $par['p_lat_body']);
                post('lat-finale', $par['p_lat_finale']);
            } else {
                $this->redirect()->setMessage($f->getMessages());
            }
            $this->render('files/View/Member/Profil/CoupleData.inc');
            return;
        }
        $stt_amend = constant('self::AMEND_' . post('stt-trida'));
        $stt_bonus = constant('self::BONUS_' . post('stt-trida'));
        $lat_amend = constant('self::AMEND_' . post('lat-trida'));
        $lat_bonus = constant('self::BONUS_' . post('lat-trida'));

        $stt_body_capped = post('stt-body') > 200 ? 200 : post('stt-body');
        $lat_body_capped = post('lat-body') > 200 ? 200 : post('lat-body');
        
        $hodnoceni =
            ($stt_body_capped + 40 * post('stt-finale')) * $stt_amend +
            ($lat_body_capped + 40 * post('lat-finale')) * $lat_amend +
            $stt_bonus +
            $lat_bonus;

        DBPary::editTridaBody(
            User::getParID(),
            post('stt-trida'), post('stt-body'), post('stt-finale'),
            post('lat-trida'), post('lat-body'), post('lat-finale'),
            $hodnoceni
        );
        $this->redirect("/member/profil/par", "Třída a body změněny");
    }
    public function partner($id = null) {
        $latest = DBPary::getLatestPartner(User::getUserID(), User::getUserPohlavi());
        $havePartner = !empty($latest) && $latest['u_id'];

        if (!empty($_POST)) {
            if (!post("partner") || (post('action') == 'dumpthem' && $havePartner)) {
                DBPary::noPartner(User::getUserID());
                DBPary::noPartner($latest['u_id']);
                $this->redirect('/member/profil/par', 'Partnerství zrušeno');
            }
            if (post('partner') == $latest['u_id'] || (!post('partner') && $latest['u_id'] == '0')) {
                $this->redirect('/member/profil/par');
            }
            if (User::getUserPohlavi() == "m") {
                DBPary::newPartnerRequest(User::getUserID(),
                    User::getUserID(), post("partner"));
            } else {
                DBPary::newPartnerRequest(User::getUserID(),
                    post("partner"), User::getUserID());
            }
            $this->redirect('/member/profil/par', 'Žádost o partnerství odeslána');
        }

        post('partner', $havePartner ? $latest['u_id'] : '0');
        $this->render(
            'files/View/Member/Profil/PartnerOverview.inc',
            array(
                'havePartner' => $havePartner,
                'partnerID' => $latest['u_id'],
                'partnerFullName' => $latest['u_jmeno'] . ' ' . $latest['u_prijmeni'],
                'users' => (User::getUserPohlavi() == "m")
                            ? DBUser::getUsersByPohlavi("f")
                            : DBUser::getUsersByPohlavi("m")
            )
        );
    }
    public function zadost($id = null) {
        if (!post('action')) {
            $this->redirect('/member/profil');
        }
        switch(post('action')) {
            case 'accept':
            case 'refuse':
                $requests = DBPary::getPartnerRequestsForMe(User::getUserID());
                foreach ($requests as $req) {
                    if ($req['pn_id'] != post('id')) {
                        continue;
                    }

                    if (post('action') == 'accept') {
                        DBPary::acceptPartnerRequest(post('id'));
                        $this->redirect()->setMessage('žádost přijata');
                    } else {
                        DBPary::deletePartnerRequest(post('id'));
                        $this->redirect()->setMessage('žádost zamitnuta');
                    }
                    $this->redirect('/member/profil/par');
                }
                break;
            case 'cancel':
                $requests = DBPary::getPartnerRequestsByMe(User::getUserID());
                foreach ($requests as $req) {
                    if ($req['pn_id'] != post('id')) {
                        continue;
                    }
                    DBPary::deletePartnerRequest(post('id'));
                    $this->redirect('/member/profil/par', 'Žádost zrušena');
                }
                break;
            default:
                $this->redirect('/member/profil');
                break;
        }
        $this->redirect('/member/profil', 'Žádná taková žádost tu není');
    }
    private function _checkData() {
        $f = new Form();
        $f->checkInArray(
            post('stt-trida'),
            array('Z', 'H', 'D', 'C', 'B', 'A', 'M'),
            'Neplatná standartní třída', 'stt-trida'
        );
        $f->checkInArray(
            post('lat-trida'),
            array('Z', 'H', 'D', 'C', 'B', 'A', 'M'),
            'Neplatná latinská třída', 'lat-trida'
        );
        $f->checkNumberBetween(post('stt-body'), 0, 1000, 'Špatný počet standartních bodů', 'stt-body');
        $f->checkNumberBetween(post('lat-body'), 0, 1000, 'Špatný počet latinských bodů', 'lat-body');
        $f->checkNumberBetween(post('stt-finale'), 0, 10, 'Špatný počet standartních finálí', 'stt-finale');
        $f->checkNumberBetween(post('lat-finale'), 0, 10, 'Špatný počet latinských finálí', 'lat-finale');
        return $f->isValid() ? null : $f;
    }
}
?>