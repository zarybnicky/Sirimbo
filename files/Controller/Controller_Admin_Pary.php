<?php
class Controller_Admin_Pary extends Controller_Abstract
{
    public function __construct()
    {
        Permissions::checkError('pary', P_OWNED);
    }

    public function view($request)
    {
        switch ($request->post("action")) {
            case 'add':
                if ($request->post("add_partner")) {
                    DBPary::newCouple($request->post("add_partner"), $request->post("add_partnerka"));
                }
                $this->redirect('/admin/pary');
                break;

            case 'fix_unpaired':
                $xs = DBPary::getUnpairedUsers();
                foreach ($xs as $x) {
                    DBPary::noPartner($x['u_id']);
                }
                $this->redirect()->info(count($xs) . ' chybných záznamů opraveno');
                $this->redirect('/admin/pary');
                break;
        }

        $data = array_map(
            function ($item) {
                return [
                    'buttons' => $this->editLink('/admin/pary/edit/' . $item['p_id'])
                        . '&nbsp;&nbsp;'
                        . $this->removeLink('/admin/pary/remove/' . $item['p_id']),
                    'fullNameMan' => $item['guy_surname'] . ', ' . $item['guy_name'],
                    'fullNameWoman' => $item['gal'] ? ($item['gal_surname'] . ', ' . $item['gal_name']) : '',
                    'standard' => $item['p_stt_trida'] . ' ' . $item['p_stt_body'] . 'F' . $item['p_stt_finale'],
                    'latina' => $item['p_lat_trida'] . ' ' . $item['p_lat_body'] . 'F' . $item['p_lat_finale'],
                    'hodnoceni' => $item['p_hodnoceni']
                ];
            },
            DBPary::getActivePary()
        );

        $this->render('files/View/Admin/Pary/Overview.inc', [
            'header' => 'Správa párů',
            'data' => $data,
            'usersMen' => DBUser::getUsersByPohlavi('m'),
            'usersWomen' => DBUser::getUsersByPohlavi('f')
        ]);
    }

    public function edit($request)
    {
        if (!$id = $request->getId()) {
            $this->redirect()->warning('Pár s takovým ID neexistuje');
            $this->redirect('/admin/pary');
        }
        if (!$data = DBPary::getSinglePar($id)) {
            $this->redirect()->warning('Pár s takovým ID neexistuje');
            $this->redirect('/admin/pary');
        }

        if (!$request->post()) {
            $this->render('files/View/Admin/Pary/Form.inc', [
                'header' => 'Správa párů',
                'subheader' => 'Změna třídy a bodů',
                'fullName' => (
                    $data['guy_name'] . ' ' . $data['guy_surname'] . ' - '
                    . $data['gal_name'] . ' ' . $data['gal_surname']
                ),
                'stt_trida' => $data['p_stt_trida'],
                'stt_body' => $data['p_stt_body'],
                'stt_finale' => $data['p_stt_finale'],
                'lat_trida' => $data['p_lat_trida'],
                'lat_body' => $data['p_lat_body'],
                'lat_finale' => $data['p_lat_finale']
            ]);
            return;
        }
        $stt_body =
            ($request->post('stt-body') && is_numeric($request->post('stt-body')))
            ? $request->post('stt-body') : 0;
        $stt_body_capped = max($stt_body, 200);
        $stt_finale =
            ($request->post('stt-finale') && is_numeric($request->post('stt-finale')))
            ? $request->post('stt-finale') : 0;
        $lat_body =
            ($request->post('lat-body') && is_numeric($request->post('lat-body')))
            ? $request->post('lat-body') : 0;
        $lat_body_capped = max($lat_body, 200);
        $lat_finale =
            ($request->post('lat-finale') && is_numeric($request->post('lat-finale')))
            ? $request->post('lat-finale') : 0;

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
        $this->redirect()->success('Třída a body změněny');
        $this->redirect('/admin/pary');
    }

    public function remove($request)
    {
        $id = $request->getId();
        if ($id) {
            DBPary::removeCouple($id);
        }
        $this->redirect()->success('Pár odstraněn');
        $this->redirect('/admin/pary');
    }
}
