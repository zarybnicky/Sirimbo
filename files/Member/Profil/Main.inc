<?php
User::checkPermissionsError(L_HOST);
header_main("Profil");

notice(View::getRedirectMessage());

switch(Request::getAction()) {
	case 'profil':
		include("files/Member/Profil/DisplayMain.inc");
		return;
	case 'edit':
		$data = DBUser::getUserData(User::getUserID());
		
		if(empty($_POST)) {
			post("login", User::getUserName());
			post("level", $data["u_level"]);
			switch($data["u_level"]) {
				case L_USER:	post("level", "user"); break;
				case L_EDITOR:	post("level", "editor"); break;
				case L_TRENER:	post("level", "trener"); break;
				case L_ADMIN:	post("level", "admin"); break;
				case L_SADMIN:	post("level", "sadmin"); break;
				default:		post("level", "user"); break;
			}
			post("lock", $data["u_lock"]);
			post("jmeno", $data["u_jmeno"]);
			post("prijmeni", $data["u_prijmeni"]);
			post("pohlavi", $data["u_pohlavi"]);
			post("email", $data["u_email"]);
			post("telefon", $data["u_telefon"]);
			post('narozeni', $data['u_narozeni']);
			post("poznamky", $data["u_poznamky"]);
			
			include("files/Member/Profil/FormPerson.inc");
			return;
		}
		$narozeni = Helper::get()->date()->name('narozeni')->getPost();
		
		$f = new Form();
		$f->checkDate($narozeni, 'Neplatné datum narození', 'narozeni');
		$f->checkLength(post('jmeno'), 1, 40, 'Špatná délka jména', 'jmeno');
		$f->checkLength(post('prijmeni'), 1, 40, 'Špatná délka přijmení', 'prijmeni');
		$f->checkInArray(post('pohlavi'), array('m', 'f'), 'Neplatné pohlaví', 'pohlavi');
		$f->checkEmail(post('email'), 'Neplatný formát emailu', 'email');
		$f->checkPhone(post('telefon'), 'Neplatný formát telefoního čísla', 'telefon');
		
		if(!$f->isValid()) {
			include('files/Member/Profil/FormPerson.inc');
			return;
		}
		DBUser::setUserData(User::getUserID(), post('jmeno'), post('prijmeni'),
			post('pohlavi'), post('email'), post('telefon'), $narozeni,
			post('poznamky'), $data['u_level'], $data['u_skupina'],
			$data['u_dancer'], $data['u_lock'], $data['u_ban'], $data['u_system']);
		
		View::redirect('/member/profil', 'Upraveno');
		return;
		break;
	
	case 'heslo':
		if(empty($_POST)) {
			include('files/Member/Profil/FormPass.inc');
			return;
		}
		$f = new Form();
		$f->checkPassword(post('newpass'), 'Neplatný formát hesla', 'newpass');
		$f->checkBool(DBUser::checkUser(User::getUserName(), User::Crypt(post('oldpass'))),
			'Staré heslo je špatně', 'oldpass');
		$f->checkBool(post('newpass') == post('newpass_confirm'),
			'Hová hesla se neshodují', 'newpass_check');
		
		if(!$f->isValid()) {
			include('files/Member/Profil/FormPass.inc');
			return;
		}
		DBUser::setPassword(User::getUserID(), User::Crypt(post('newpass')));
		View::redirect('/member/profil', 'Heslo změněno');
		return;
	case 'platby':
		include('files/Member/Profil/DisplayPlatby.inc');
		return;
}
?>