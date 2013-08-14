<?php
include_once('files/Controller/Member.php');
class Controller_Member_Akce extends Controller_Member {
	function __construct() {
		Permissions::checkError('akce', P_VIEW);
	}
	function view($id = null) {
		if($id) {
			DisplayAkce::viewFullAkce($id);
			return;
		}
		$akce = DBAkce::getAkce(true);
		if(empty($akce)) {
			notice('Žádné akce k dispozici');
			return;
		}
		if(!empty($_POST) && post('id') &&
				($data = DBAkce::getSingleAkce(post('id'))) &&
				!is_object($f = $this->checkData($data, post('action')))) {
			if(post('action') == 'signup') {
				DBAkce::signUp(User::getUserID(), post('id'), User::getDatumNarozeni());
			} elseif(post('action') == 'signout') {
				DBAkce::signOut(User::getUserID(), post('id'));
			}
		}
		include('files/Member/Akce.inc');
	}
	private function checkData($data, $action) {
		$f = new Form();
		$f->checkBool(!$data['a_lock'], 'Tato akce je zamčená', '');
		$f->checkInArray($action, array('signup', 'signout'), 'Špatná akce', '');
		$f->checkNumeric(post('id'), 'Špatné ID', '');
		
		return $f->isValid() ? true : $f;
	}
}
?>