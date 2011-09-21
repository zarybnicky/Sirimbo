<?php
User::checkPermissionsError(L_EDITOR);
header_main("Profil");

if(empty($_POST)) {
	include("files/Member/Profil/DisplayMain.inc");
	return;
}

switch(getPostField("action")) {
	
	case "edit-per":
		$userData = DBUser::getUserData(User::getUserName());
		dumpVar($userData);
		$_POST["login"] = User::getUserID();
		$_POST["level"] = $userData["u_level"];
		switch($userData["u_level"]) {
			case L_USER:
				$_POST["level"] = "user";	break;
			case L_EDITOR:
				$_POST["level"] = "editor";	break;
			case L_TRENER:
				$_POST["level"] = "trener";	break;
			case L_ADMIN:
				$_POST["level"] = "admin";	break;
			case L_SADMIN:
				$_POST["level"] = "sadmin";	break;
			default:
				$_POST["level"] = "user";	break;
		}
		$_POST["lock"] = $userData["u_lock"];
		$_POST["jmeno"] = $userData["u_jmeno"];
		$_POST["prijmeni"] = $userData["u_prijmeni"];
		$_POST["email"] = $userData["u_email"];
		$_POST["telefon"] = $userData["u_telefon"];
		$_POST["poznamky"] = $userData["u_poznamky"];
		
		include("files/Member/Profil/Form.inc");
		return;
		break;
		
	case "edit-per_confirm":
		$data = DBUser::getUserData(User::getUserName());
		
		$login = User::getUserName();
		$level = $data["u_level"];
		$lock = $data["u_lock"];
		$ban = $data["u_ban"];
		$jmeno = getPostField("jmeno");
		$prijmeni = getPostField("prijmeni");
		$email = getPostField("email");
		$telefon = getPostField("telefon");
		$poznamky = getPostField("poznamky");
		
		$error = false;
		
		ob_start();
		include("files/Member/Profil/Form.inc");
		$html = ob_get_clean();
		
		if(!$error) {
			DBUser::setUserData($login, $level, $lock, $ban, $jmeno, $prijmeni, $email, $telefon, $poznamky);
			notice("Upraveno");
		} else {
			echo $html;
			return;
		}
		break;
	
	case "edit-pas":
		
		break;
}

include("files/Member/Profil/DisplayMain.inc");
?>