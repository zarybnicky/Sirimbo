<?php
class Controller_Member_Nabidka implements Controller_Interface  {
	function __construct() {
		Permissions::checkError('dokumenty', P_VIEW);
	}
	function view($id = null) {
		if(empty($_POST)) {
			include('files/Member/Nabidka.inc');
			return;
		}
		$data = DBNabidka::getSingleNabidka(post('id'));
		
		if(post('hodiny') > 0 && !is_object($this->checkData($data, 'signup'))) {
			$partnerka = DBUser::getUserData(User::getPartnerID());
			
			if(!User::getZaplaceno() || (User::getPartnerID() > 0 &&
					!(strcmp($partnerka['up_plati_do'], date('Y-m-d', strtotime('+ 14 days'))) >= 0))) {
				notice('Buď vy nebo váš partner(ka) nemáte zaplacené členské příspěvky');
			} elseif($data['n_max_pocet_hod'] > 0 &&
					(DBNabidka::getNabidkaLessons(post('id'), User::getID()) + post('hodiny')) > $data['n_max_pocet_hod']) {
				notice('Maximální počet hodin na pár je ' . $data['n_max_pocet_hod'] . '!');
			} elseif(($data['n_pocet_hod'] - DBNabidka::getNabidkaItemLessons(post('id'))) < post('hodiny')) {
				notice('Tolik volných hodin tu není');
			} else {
				DBNabidka::addNabidkaItemLessons(User::getParID(), post('id'), post('hodiny'));
				notice('Hodiny přidány');
				
				unset($_POST['hodiny']);
			}
		} elseif(post('un_id') !== null && !is_object($this->checkData($data, 'signoff'))) {
			list($u_id, $n_id) = explode('-', post('un_id'));
			
			if(!DBNabidka::getNabidkaLessons($n_id, $u_id)) {
				notice('Neplatný požadavek!');
			} elseif($u_id != User::getParID() && !Permissions::check('nabidka', P_OWNED, $data['n_trener'])) {
				notice('Nedostatečná oprávnění!');
			} else {
				DBNabidka::removeNabidkaItem($n_id, $u_id);
				notice('Hodiny odebrány');
			}
		}
		include('files/Member/Nabidka.inc');
	}
	private function checkData($data, $action = 'signup') {
		$f = new Form();
		$f->checkBool(!$data['n_lock'], 'Tato nabídka je uzamčená', '');
		if(post('hodiny'))
			$f->checkNumeric(post('hodiny'), 'Špatný počet hodin', 'hodiny');
	}
}
?>