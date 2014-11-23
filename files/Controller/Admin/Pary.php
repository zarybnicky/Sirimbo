<?php
require_once 'files/Controller/Admin.php';
class Controller_Admin_Pary extends Controller_Admin
{
    public function __construct() {
        Permissions::checkError('pary', P_OWNED);
    }
    public function view($request) {
        switch(post("action")) {
            case "remove":
                if (!is_array(post("pary")))
                    break;
                list($par) = post("pary");
                $data = DBPary::getSinglePar($par);

                if ($data['guy_id'])
                    DBPary::noPartner($data['guy_id']);
                if ($data['gal_id'])
                    DBPary::noPartner($data['gal_id']);

                $this->redirect('/admin/pary', 'Pár odstraněn');
                break;

            case 'add':
                $old_gal = DBPary::getLatestPartner(post("add_partner"), 'm');
                $old_guy = DBPary::getLatestPartner(post("add_partnerka"), 'f');

                if (post("add_partner"))
                    DBPary::newPartner(post("add_partner"), post("add_partnerka"));
                if ($old_guy['u_id'])
                    DBPary::noPartner($old_guy['u_id']);
                if ($old_gal['u_id'])
                    DBPary::noPartner($old_gal['u_id']);

                $this->redirect('/admin/pary', 'Pár přidán');
                break;

            case 'edit':
                $pary = post('pary');
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
        if (!$id || !($data = DBPary::getSinglePar($id)))
            $this->redirect('/admin/pary', 'Pár s takovým ID neexistuje');

        if (empty($_POST)) {
            post('stt-trida', $data['p_stt_trida']);
            post('stt-body', $data['p_stt_body']);
            post('stt-finale', $data['p_stt_finale']);
            post('lat-trida', $data['p_lat_trida']);
            post('lat-body', $data['p_lat_body']);
            post('lat-finale', $data['p_lat_finale']);

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
            (post('stt-body') && is_numeric(post('stt-body')))
            ? post('stt-body') : 0;
        $stt_body_capped = $stt_body > 200 ? 200 : $stt_body;
        $stt_finale =
            (post('stt-finale') && is_numeric(post('stt-finale')))
            ? post('stt-finale') : 0;
        $lat_body =
            (post('lat-body') && is_numeric(post('lat-body')))
            ? post('lat-body') : 0;
        $lat_body_capped = $lat_body > 200 ? 200 : $lat_body;
        $lat_finale =
            (post('lat-finale') && is_numeric(post('lat-finale')))
            ? post('lat-finale') : 0;

        require_once 'files/Controller/Member/Profil/Par.php';
        $stt_amend = constant('Controller_Member_Profil_Par::AMEND_' . post('stt-trida'));
        $lat_amend = constant('Controller_Member_Profil_Par::AMEND_' . post('lat-trida'));
        $stt_base = ($stt_body_capped + 40 * $stt_finale) * $stt_amend;
        $lat_base = ($lat_body_capped + 40 * $lat_finale) * $lat_amend;

        $stt_bonus = constant('Controller_Member_Profil_Par::BONUS_' . post('stt-trida'));
        $lat_bonus = constant('Controller_Member_Profil_Par::BONUS_' . post('lat-trida'));

        $hodnoceni = $stt_base + $lat_base + $stt_bonus + $lat_bonus;

        DBPary::editTridaBody(
            $data['p_id'], post('stt-trida'), $stt_body,
            $stt_finale, post('lat-trida'), $lat_body, $lat_finale, $hodnoceni
        );
        $this->redirect('/admin/pary', 'Třída a body změněny');
    }
}
