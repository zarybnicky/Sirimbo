<?php
require_once 'files/Controller/Admin.php';
class Controller_Admin_Pary extends Controller_Admin
{
    public function __construct() {
        Permissions::checkError('pary', P_OWNED);
    }
    public function view($request) {
        switch($request->post("action")) {
        case "remove":
            if (!is_array($request->post("pary"))) {
                break;
            }
            list($par) = $request->post("pary");
            $data = DBPary::getSinglePar($par);

            if ($data['guy_id']) {
                DBPary::noPartner($data['guy_id']);
            }
            if ($data['gal_id']) {
                DBPary::noPartner($data['gal_id']);
            }

            $this->redirect('/admin/pary', 'Pár odstraněn');
            break;

        case 'add':
            $old_gal = DBPary::getLatestPartner($request->post("add_partner"), 'm');
            $old_guy = DBPary::getLatestPartner($request->post("add_partnerka"), 'f');

            if ($request->post("add_partner")) {
                DBPary::newPartner($request->post("add_partner"), $request->post("add_partnerka"));
            }
            if ($old_guy['u_id']) {
                DBPary::noPartner($old_guy['u_id']);
            }
            if ($old_gal['u_id']) {
                DBPary::noPartner($old_gal['u_id']);
            }

            $this->redirect('/admin/pary', 'Pár přidán');
            break;

        case 'edit':
            $pary = $request->post('pary');
            if ($pary[0])
                $this->redirect('/admin/pary/edit/' . $pary[0]);
            else
                $this->redirect('/admin/pary');
            break;
        }

        $data = array_map(
            function ($item) {
                return array(
                    'checkBox' => $this->checkbox('pary[]', $item['p_id'])->render(),
                    'fullNameMan' => $item['guy_surname'] . ', ' . $item['guy_name'],
                    'fullNameWoman' => $item['gal'] ? ($item['gal_surname'] . ', ' . $item['gal_name']) : '',
                    'standart' => $item['p_stt_trida'] . ' ' . $item['p_stt_body'] . 'F' . $item['p_stt_finale'],
                    'latina' => $item['p_lat_trida'] . ' ' . $item['p_lat_body'] . 'F' . $item['p_lat_finale'],
                    'hodnoceni' => $item['p_hodnoceni']
                );
            },
            DBPary::getActivePary()
        );

        $this->render(
            'files/View/Admin/Pary/Overview.inc',
            array(
                'showMenu' => !TISK,
                'data' => $data,
                'usersMen' => DBUser::getUsersByPohlavi('m'),
                'usersWomen' => DBUser::getUsersByPohlavi('f')
            )
        );
    }
    public function edit($request) {
        $id = $request->getId();
        if (!$id || !($data = DBPary::getSinglePar($id))) {
            $this->redirect('/admin/pary', 'Pár s takovým ID neexistuje');
        }

        if (!$request->post()) {
            $request->post('stt-trida', $data['p_stt_trida']);
            $request->post('stt-body', $data['p_stt_body']);
            $request->post('stt-finale', $data['p_stt_finale']);
            $request->post('lat-trida', $data['p_lat_trida']);
            $request->post('lat-body', $data['p_lat_body']);
            $request->post('lat-finale', $data['p_lat_finale']);

            $this->render(
                'files/View/Admin/Pary/Form.inc',
                array(
                    'fullName' =>
                        $data['guy_name'] . ' ' . $data['guy_surname'] . ' - '
                        . $data['gal_name'] . ' ' . $data['gal_surname']
                )
            );
            return;
        }
        $stt_body =
            ($request->post('stt-body') && is_numeric($request->post('stt-body')))
            ? $request->post('stt-body') : 0;
        $stt_body_capped = $stt_body > 200 ? 200 : $stt_body;
        $stt_finale =
            ($request->post('stt-finale') && is_numeric($request->post('stt-finale')))
            ? $request->post('stt-finale') : 0;
        $lat_body =
            ($request->post('lat-body') && is_numeric($request->post('lat-body')))
            ? $request->post('lat-body') : 0;
        $lat_body_capped = $lat_body > 200 ? 200 : $lat_body;
        $lat_finale =
            ($request->post('lat-finale') && is_numeric($request->post('lat-finale')))
            ? $request->post('lat-finale') : 0;

        require_once 'files/Controller/Member/Profil/Par.php';
        $stt_amend = constant('Controller_Member_Profil_Par::AMEND_' . $request->post('stt-trida'));
        $lat_amend = constant('Controller_Member_Profil_Par::AMEND_' . $request->post('lat-trida'));
        $stt_base = ($stt_body_capped + 40 * $stt_finale) * $stt_amend;
        $lat_base = ($lat_body_capped + 40 * $lat_finale) * $lat_amend;

        $stt_bonus = constant('Controller_Member_Profil_Par::BONUS_' . $request->post('stt-trida'));
        $lat_bonus = constant('Controller_Member_Profil_Par::BONUS_' . $request->post('lat-trida'));

        $hodnoceni = $stt_base + $lat_base + $stt_bonus + $lat_bonus;

        DBPary::editTridaBody(
            $data['p_id'],
            $request->post('stt-trida'),
            $stt_body,
            $stt_finale,
            $request->post('lat-trida'),
            $lat_body,
            $lat_finale,
            $hodnoceni
        );
        $this->redirect('/admin/pary', 'Třída a body změněny');
    }
}
