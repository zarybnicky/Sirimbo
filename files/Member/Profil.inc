<?php
User::checkPermissionsError(L_HOST);
header_main("Profil");

if(empty($_POST)) {
	DisplayPary::viewPartnerRequests(DBPary::getPartnerRequestsForMe(User::getUserID()),
		DBPary::getPartnerRequestsByMe(User::getUserID()));
	include("files/Member/Profil/DisplayMain.inc");
	return;
}

switch(getPostField("action")) {
	
	case "edit-per":
		$userData = DBUser::getUserData(User::getUserName());
		
		$_POST["login"] = User::getUserName();
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
		$_POST["pohlavi"] = $userData["u_pohlavi"];
		$_POST["email"] = $userData["u_email"];
		$_POST["telefon"] = $userData["u_telefon"];
		$_POST["poznamky"] = $userData["u_poznamky"];
		
		include("files/Member/Profil/FormPerson.inc");
		return;
		break;
		
	case 'edit-per_confirm':
		$data = DBUser::getUserData(User::getUserName());
		
		$login = User::getUserName();
		$level = $data['u_level'];
		$lock = $data['u_lock'];
		$ban = $data['u_ban'];
		$jmeno = getPostField('jmeno');
		$prijmeni = getPostField('prijmeni');
		$pohlavi = getPostField('pohlavi');
		$email = getPostField('email');
		$telefon = getPostField('telefon');
		$poznamky = getPostField('poznamky');
		
		$error = false;
		
		ob_start();
		include('files/Member/Profil/FormPerson.inc');
		$html = ob_get_clean();
		
		if(!$error) {
			DBUser::setUserData($login, $jmeno, $prijmeni, $pohlavi, $email, $telefon,
				$poznamky, $level, $lock, $ban);
			notice('Upraveno');
		} else {
			echo $html;
			return;
		}
		break;
	
	case 'edit-pass':
		include('files/Member/Profil/FormPass.inc');
		return;
		break;
		
	case 'edit-pass_confirm':
		$oldpass_check = DBUser::checkUser(User::getUserName(), User::Crypt(getPostField('oldpass')));
		$newpass_check = getPostField('newpass') == getPostField('newpass_confirm');
		
		ob_start();
		include('files/Member/Profil/FormPass.inc');	
		$html = ob_get_clean();
		
		if(!$error && $oldpass_check && $newpass_check) {
			DBUser::setPassword(User::getUserID(), User::Crypt(getPostField('newpass')));
			notice('Heslo změněno');
		} elseif(!$oldpass_check) {
			notice('Staré heslo je špatně, zkus to znova :o(');
			include('files/Member/Profil/FormPass.inc');
			return;
		} elseif(!$newpass_check) {
			notice('Nová hesla nejsou stejná, zkus to znova :o(');
			include('files/Member/Profil/FormPass.inc');
			return;
		} elseif($error) {
			echo $html;
			return;
		}
		break;
		
	case 'mate':
		DisplayPary::viewPartnerRequests(DBPary::getPartnerRequestsForMe(User::getUserID()),
			DBPary::getPartnerRequestsByMe(User::getUserID()));
		
		$latest = DBPary::getLatestPartner(User::getUserID(), User::getUserPohlavi());
		$gotPartner = !empty($latest) && $latest['u_id'] > 0;
		if($gotPartner) {
			echo "Právě tančím s: ";
			echoFullJmeno($latest);
		} else {
			echo 'Právě s nikým netančím';
		}
		echo '<form method="POST" action="' . $_SERVER['REQUEST_URI'] . '">';
		echo '<button type="submit" name="action" value="edit-mate">Změnit partnera/ku</button>';
		if($gotPartner)
			echo '<button type="submit" name="action" value="edit-mate-dumpthem">Rozejít se</button>';
		echo '<button type="submit" name="action" value="">Zpět</button>';
		echo '</form>';
		return;
		break;
	
	case 'edit-mate':
		DisplayPary::viewPartnerRequests(DBPary::getPartnerRequestsForMe(User::getUserID()),
			DBPary::getPartnerRequestsByMe(User::getUserID()));
			
		$latest = DBPary::getLatestPartner(User::getUserID(), User::getUserPohlavi());
		$gotPartner = !empty($latest) && $latest['u_id'] > 0;
		if($gotPartner) {
			echo "Právě tančím s: ";
			echoFullJmeno($latest);
			$id = $latest['u_id'];
		} else {
			echo 'Právě s nikým netančím';
			$id = "none";
		}
		
		$_POST["partner"] = $id;
		if(User::getUserPohlavi() == "m") {
			$partners = DBUser::getUsersByPohlavi("f");
		} else {
			$partners = DBUser::getUsersByPohlavi("m");
		}
		
		echo '<form method="POST" action="' . $_SERVER['REQUEST_URI'] . '">';
		echoUsers("partner", $partners);
		echo '<button type="submit" name="action" value="edit-mate_confirm">Požádat o partnerství</button>';
		if($gotPartner)
			echo '<button type="submit" name="action" value="edit-mate-dumpthem">Rozejít se</button>';
		echo '<button type="submit" name="action" value="mate">Zpět</button>';
		echo '</form>';
		return;
		break;
		
	case 'edit-mate_confirm':
		$partner = DBPary::getLatestPartner(User::getUserID(), User::getUserPohlavi());
		
		if(getPostField('partner') == $partner['u_id'] ||
				(getPostField('partner') == "none" && $partner['u_id'] == '0')) {
			notice('Nic se nezměnilo.');
			break;
		}

		if(getPostField("partner") == "none") {
			DBPary::noPartner(User::getUserID());
			DBPary::noPartner($partner['u_id']);
			notice('A zase sám a sám...');
		} else {
			if(User::getUserPohlavi() == "m") {
				DBPary::newPartnerRequest(User::getUserID(), User::getUserID(), getPostField("partner"));
			} else {
				DBPary::newPartnerRequest(User::getUserID(), getPostField("partner"), User::getUserID());
			}
			notice("Žádost o partnerství odeslána");
		}
		break;
		
	case 'edit-mate-dumpthem':
		$partner = DBPary::getLatestPartner(User::getUserID(), User::getUserPohlavi());
		if(!empty($partner) && $partner['u_id'] > 0) {
			DBPary::noPartner(User::getUserID());
			DBPary::noPartner($partner['u_id']);
		}
		notice('A zase sám a sám...');
		break;
	
	case 'mate-accept':
		$requests = DBPary::getPartnerRequestsForMe(User::getUserID());
		foreach($requests as $req) {
			if($req['pn_id'] == getPostField('id')) {
				DBPary::acceptPartnerRequest(getPostField('id'));
				notice('Žádost přijata');
				break 2;
			}
		}
		notice('Žádná taková žádost tu není');
		break;
	case 'mate-refuse':
		$requests = DBPary::getPartnerRequestsForMe(User::getUserID());
		foreach($requests as $req) {
			if($req['pn_id'] == getPostField('id')) {
				DBPary::deletePartnerRequest(getPostField('id'));
				notice('Žádost zamítnuta');
				break 2;
			}
		}
		notice('Žádná taková žádost tu není');
		break;
	case 'mate-cancel':
		$requests = DBPary::getPartnerRequestsByMe(User::getUserID());
		foreach($requests as $req) {
			if($req['pn_id'] == getPostField('id')) {
				DBPary::deletePartnerRequest(getPostField('id'));
				notice('Žádost zrušena');
				break 2;
			}
		}
		notice('Žádná taková žádost tu není');
		break;
}
DisplayPary::viewPartnerRequests(DBPary::getPartnerRequestsForMe(User::getUserID()),
	DBPary::getPartnerRequestsByMe(User::getUserID()));
include('files/Member/Profil/DisplayMain.inc');
?>