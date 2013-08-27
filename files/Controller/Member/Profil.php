<?php
include_once('files/Controller/Member.php');
class Controller_Member_Profil extends Controller_Member {
	function __construct() {
		Permissions::checkError('nastenka', P_VIEW);
	}
	function view($id = null) {
		$this->render('files/View/Member/Profil/Overview.inc');
	}
	function edit($id = null) {
		$data = DBUser::getUserData(User::getUserID());
		$narozeni = $this->date('narozeni')->getPost();
		
		if(empty($_POST) || is_object($f = $this->__checkData('edit', $narozeni))) {
			if(empty($_POST)) {
				post("login", User::getUserName());
				post("group", $data["u_group"]);
				post("lock", $data["u_lock"]);
				post("jmeno", $data["u_jmeno"]);
				post("prijmeni", $data["u_prijmeni"]);
				post("pohlavi", $data["u_pohlavi"]);
				post("email", $data["u_email"]);
				post("telefon", $data["u_telefon"]);
				post('narozeni', $data['u_narozeni']);
				post("poznamky", $data["u_poznamky"]);
			} else {
				$this->redirect()->setRedirectMessage($f->getMessages());
			}
			$this->render("files/View/Member/Profil/PersonalData.inc");
			return;
		}
		DBUser::setUserData(User::getUserID(), post('jmeno'), post('prijmeni'),
			post('pohlavi'), post('email'), post('telefon'), $narozeni,
			post('poznamky'), $data['u_group'], $data['u_skupina'],
			$data['u_dancer'], $data['u_lock'], $data['u_ban'], $data['u_system']);
		$this->redirect('/member/profil', 'Upraveno');
	}
	function heslo($id = null) {
		if(empty($_POST) || is_object($f = $this->__checkData('heslo'))) {
			if(!empty($_POST)) {
				$this->redirect()->setRedirectMessage($f->getMessages());
			}
			$this->render('files/View/Member/Profil/NewPassword.inc');
			return;
		}
		DBUser::setPassword(User::getUserID(), User::Crypt(post('newpass')));
		$this->redirect('/member/profil', 'Heslo změněno');
	}
	function platby($id = null) {
		$platby = DBPlatby::getPlatbyFromUser(User::getUserID());
		foreach($platby as &$row) {
			$new_data = array(
					'colorBox' => getColorBox($row['us_color'], $row['us_popis']),
					'obdobi' => Settings::$platby_obdobi[$row['up_obdobi']][3],
					'castka' => $row['up_castka'],
					'datum' => formatDate($row['up_placeno']),
					'platnost' => formatDate($row['up_plati_do'])
			);
			$row = $new_data;
		}unset($row);
		
		$skupiny = DBSkupiny::getSkupiny();
		foreach($skupiny as $key => &$row) {
			if($row['us_platba_mesic'] == 0 && $row['us_platba_ctvrtrok'] == 0 && $row['us_platba_pulrok'] == 0) {
				unset($skupiny[$key]);
				continue;
			}
			$new_data = array(
					'colorBox' => getColorBox($row['us_color'], $row['us_popis']),
					'popis' => $row['us_popis'],
					'castkaCtvrtleti' => $row['us_platba_ctvrtrok'],
					'castkaPololeti' => $row['us_platba_pulrok']
			);
			$row = $new_data;
		}
		$skupina = User::getSkupinaData();
		$this->render('files/View/Member/Profil/Platby.inc', array(
				'colorBox' => getColorBox($skupina['us_color'], $skupina['us_popis']),
				'skupinaData' => $skupina['us_popis'],
				'varSymbol' => User::var_symbol(User::getUserID()),
				'zaplaceno' => User::getZaplaceno(),
				'platby' => $platby,
				'skupiny' => $skupiny
		));
	}
	private function __checkData($action, $narozeni = null) {
		$f = new Form();
		if($action == 'edit') {
			$f->checkDate($narozeni, 'Neplatné datum narození', 'narozeni');
			$f->checkLength(post('jmeno'), 1, 40, 'Špatná délka jména', 'jmeno');
			$f->checkLength(post('prijmeni'), 1, 40, 'Špatná délka přijmení', 'prijmeni');
			$f->checkInArray(post('pohlavi'), array('m', 'f'), 'Neplatné pohlaví', 'pohlavi');
			$f->checkEmail(post('email'), 'Neplatný formát emailu', 'email');
			$f->checkPhone(post('telefon'), 'Neplatný formát telefoního čísla', 'telefon');
		} elseif($action == 'heslo') {
			$f->checkPassword(post('newpass'), 'Neplatný formát hesla', 'newpass');
			$f->checkBool(DBUser::checkUser(User::getUserName(), User::Crypt(post('oldpass'))),
					'Staré heslo je špatně', 'oldpass');
			$f->checkBool(post('newpass') == post('newpass_confirm'),
					'Nová hesla se neshodují', 'newpass_check');
		}
		return $f->isValid() ? null : $f;
	}
}
?>