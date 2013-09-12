<?php
include_once('files/Controller/Admin.php');
class Controller_Admin_Pary extends Controller_Admin {
	function __construct() {
		Permissions::checkError('pary', P_OWNED);
	}
	function view($id = null) {
		switch(post("action")) {
			case "remove":
				if(!is_array(post("pary")))
					break;
				list($par) = post("pary");
				$data = DBPary::getSinglePar($par);
				
				if($data['guy_id'])
					DBPary::noPartner($data['guy_id']);
				if($data['gal_id'])
					DBPary::noPartner($data['gal_id']);
				
				$this->redirect()->setMessage('Pár odstraněn');
				break;
			case 'add':
				$old_gal = DBPary::getLatestPartner(post("add_partner"), 'm');
				$old_guy = DBPary::getLatestPartner(post("add_partnerka"), 'f');
				
				if(post("add_partner"))
					DBPary::newPartner(post("add_partner"), post("add_partnerka"));
				if($old_guy['u_id'])
					DBPary::noPartner($old_guy['u_id']);
				if($old_gal['u_id'])
					DBPary::noPartner($old_gal['u_id']);
				
				$this->redirect()->setMessage('Pár přidán');
				break;
			case 'edit':
				$pary = post('pary');
				if($pary[0])
					$this->redirect('/admin/pary/edit/' . $pary[0]);
				break;
		}
		$data = DBPary::getActivePary();
		foreach($data as &$row) {
			$new_data = array(
					'checkBox' => '<input type="checkbox" name="pary[]" value="' . $row["p_id"] . '" />',
					'fullNameMan' => $row['guy_surname'] . ', ' . $row['guy_name'],
					'fullNameWoman' => $row['gal'] ? ($row['gal_surname'] . ', ' . $row['gal_name']) : '',
					'standart' => $row['p_stt_trida'] . ' ' . $row['p_stt_body'] . 'F' . $row['p_stt_finale'],
					'latina' => $row['p_lat_trida'] . ' ' . $row['p_lat_body'] . 'F' . $row['p_lat_finale'],
					'hodnoceni' => $row['p_hodnoceni']
			);
			$row = $new_data;
		}
		$this->render("files/View/Admin/Pary/Overview.inc", array(
				'showMenu' => !TISK,
				'data' => $data,
				'usersMen' => DBUser::getUsersByPohlavi('m'),
				'usersWomen' => DBUser::getUsersByPohlavi('f')
		));
	}
	function edit($id = null) {
		if(!$id || !($data = DBPary::getSinglePar($id)))
			$this->redirect('/admin/pary', 'Pár s takovým ID neexistuje');

		if(empty($_POST)) {
			post('stt-trida', $data['p_stt_trida']);
			post('stt-body', $data['p_stt_body']);
			post('stt-finale', $data['p_stt_finale']);
			post('lat-trida', $data['p_lat_trida']);
			post('lat-body', $data['p_lat_body']);
			post('lat-finale', $data['p_lat_finale']);
			
			$this->render('files/View/Admin/Pary/Form.inc', array(
					'fullName' => $data['guy_name'] . ' ' . $data['guy_surname'] . ' - ' .
						$data['gal_name'] . ' ' . $data['gal_surname']
			));
			return;
		}
		$stt_body = (post('stt-body') && is_numeric(post('stt-body'))) ?
			post('stt-body') : 0;
		$stt_finale = (post('stt-finale') && is_numeric(post('stt-finale'))) ?
			post('stt-finale') : 0;
		$lat_body = (post('lat-body') && is_numeric(post('lat-body'))) ?
			post('lat-body') : 0;
		$lat_finale = (post('lat-finale') && is_numeric(post('lat-finale'))) ?
			post('lat-finale') : 0;
			
		$stt_amend = constant("AMEND_" . post('stt-trida'));
		$lat_amend = constant("AMEND_" . post('lat-trida'));
		$stt_base = ($stt_body + 40 * $stt_finale) * $stt_amend;
		$lat_base = ($lat_body + 40 * $lat_finale) * $lat_amend;
		
		$stt_bonus = constant("BONUS_" . post('stt-trida'));
		$lat_bonus = constant("BONUS_" . post('lat-trida'));
		
		$hodnoceni = $stt_base + $lat_base + $stt_bonus + $lat_bonus;
		
		DBPary::editTridaBody($data['p_id'], post('stt-trida'), $stt_body,
			$stt_finale, post('lat-trida'), $lat_body, $lat_finale, $hodnoceni);
		
		$this->redirect('/admin/pary', 'Třída a body změněny');
	}
}
?>