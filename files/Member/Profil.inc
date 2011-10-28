<?php
User::checkPermissionsError(L_HOST);
header_main("Profil");

$request = explode('/', $_SERVER['REQUEST_URI']);
$action = array_slice($request, 3);
if(empty($action)) $action[] = '';

if(!$action[0]) {
	include("files/Member/Profil/DisplayMain.inc");
	return;
}
switch($action[0]) {
	case 'edit':
		$data = DBUser::getUserData(User::getUserID());
		
		if(empty($_POST)) {
			
			$_POST["login"] = User::getUserName();
			$_POST["level"] = $data["u_level"];
			switch($data["u_level"]) {
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
			$_POST["lock"] = $data["u_lock"];
			$_POST["jmeno"] = $data["u_jmeno"];
			$_POST["prijmeni"] = $data["u_prijmeni"];
			$_POST["pohlavi"] = $data["u_pohlavi"];
			$_POST["email"] = $data["u_email"];
			$_POST["telefon"] = $data["u_telefon"];
			$_POST["poznamky"] = $data["u_poznamky"];
			
			include("files/Member/Profil/FormPerson.inc");
			return;
		} else {
			$error = false;
			ob_start();
			include('files/Member/Profil/FormPerson.inc');
			$html = ob_get_clean();
			
			if(!$error) {
				DBUser::setUserData(User::getUserID(), getPostField('jmeno'), getPostField('prijmeni'),
					getPostField('pohlavi'), getPostField('email'), getPostField('telefon'),
					getPostField('poznamky'), $data['u_level'], $data['u_lock'], $data['u_ban']);
				notice('Upraveno');
				
				include("files/Member/Profil/DisplayMain.inc");
				return;
			} else {
				echo $html;
				return;
			}
		}
		break;
	
	case 'heslo':
		if(empty($_POST)) {
			include('files/Member/Profil/FormPass.inc');
			return;
		} else {
			$oldpass_check = DBUser::checkUser(User::getUserName(), User::Crypt(getPostField('oldpass')));
			$newpass_check = getPostField('newpass') == getPostField('newpass_confirm');
			
			ob_start();
			include('files/Member/Profil/FormPass.inc');	
			$html = ob_get_clean();
			
			if(!$error && $oldpass_check && $newpass_check) {
				DBUser::setPassword(User::getUserID(), User::Crypt(getPostField('newpass')));
				notice('Heslo změněno');
				include("files/Member/Profil/DisplayMain.inc");
				return;
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
		}
		break;
	
	case 'par':
		$latest = DBPary::getLatestPartner(User::getUserID(), User::getUserPohlavi());
		$gotPartner = !empty($latest) && $latest['u_id'] > 0;
		$vysledky = DBPary::getVysledky($latest['p_id']);
		
		if(empty($action[1])) {
			DisplayPary::viewPartnerRequests(DBPary::getPartnerRequestsForMe(User::getUserID()),
				DBPary::getPartnerRequestsByMe(User::getUserID()));
			
			if($gotPartner) {
				echo "Právě tančím s: ";
				echoFullJmeno($latest);
				echo ' <a href="/member/profil/par/partner">Změnit partnera</a><br />';
				header_minor('Třídy a body:');
				echo 
					'Standardní tance:<br />',
					'Třída: <span style="font-weight:bolder;">', $vysledky['p_stt_trida'],
					'</span>, body: ', $vysledky['p_stt_body'],
					', finále: ', $vysledky['p_stt_finale'], '<br />',
					'Latinsko-americké tance:<br />',
					'Třída: <span style="font-weight:bolder;">', $vysledky['p_lat_trida'],
					'</span>, body: ', $vysledky['p_lat_body'],
					', finále: ', $vysledky['p_lat_finale'], '<br /><br/>',
					'V "Olympáckém žebříčku" máme ', $vysledky['p_hodnoceni'], ' bodů.<br />';
				echo '<a href="/member/profil/par/body">Změnit třídu a body</a><br />';
			} else {
				echo 'Právě s nikým netančím ';
				echo '<a href="/member/profil/par/partner">Změnit partnera</a><br />';
			}
			echo '<a href="/member/profil">Zpět</a>';
			return;
		}
		switch($action[1]) {
			case 'body':
				DisplayPary::viewPartnerRequests(DBPary::getPartnerRequestsForMe(User::getUserID()),
					DBPary::getPartnerRequestsByMe(User::getUserID()));
				
				if(empty($_POST)) {
					$_POST['stt-trida'] = $vysledky['p_stt_trida'];
					$_POST['stt-body'] = $vysledky['p_stt_body'];
					$_POST['stt-finale'] = $vysledky['p_stt_finale'];
					$_POST['lat-trida'] = $vysledky['p_lat_trida'];
					$_POST['lat-body'] = $vysledky['p_lat_body'];
					$_POST['lat-finale'] = $vysledky['p_lat_finale'];
					
					include('files/Member/Profil/FormClass.inc');
					return;	
				} else {
					$stt_trida = getPostField('stt-trida');
					$lat_trida = getPostField('lat-trida');
					
					$post_stt_body = getPostField('stt-body');
					$stt_body = ($post_stt_body && $post_stt_body < 1000 && $post_stt_body > 0) ?
						$post_stt_body : 0;
						
					$post_stt_finale = getPostField('stt-finale');
					$stt_finale = ($post_stt_finale && $post_stt_finale < 1000 && $post_stt_finale > 0) ?
						$post_stt_finale : 0;
					
					$post_lat_body = getPostField('lat-body');
					$lat_body = ($post_lat_body && $post_lat_body < 1000 && $post_lat_body > 0) ?
						$post_lat_body : 0;
						
					$post_lat_finale = getPostField('lat-finale');
					$lat_finale = ($post_lat_finale && $post_lat_finale < 1000 && $post_lat_finale > 0) ?
						$post_lat_finale : 0;
					
					$stt_base = ($stt_body + 40 * $stt_finale) * constant('AMEND_' . $stt_trida);
					$lat_base = ($lat_body + 40 * $lat_finale) * constant('AMEND_' . $lat_trida);
					
					$hodnoceni = $stt_base + $lat_base +
						constant('BONUS_' . $stt_trida) + constant('BONUS_' . $lat_trida);
					
					DBPary::editTridaBody(User::getParID(), $stt_trida, $stt_body,
						$stt_finale, $lat_trida, $lat_body, $lat_finale, $hodnoceni);
					
					notice("Třída a body změněny");
					include("files/Member/Profil/DisplayMain.inc");
					return;
				}
				break;
			
			case 'partner':
				DisplayPary::viewPartnerRequests(DBPary::getPartnerRequestsForMe(User::getUserID()),
					DBPary::getPartnerRequestsByMe(User::getUserID()));
				
				if(empty($_POST)) {
					if($gotPartner) {
						echo "Právě tančím s: ";
						echoFullJmeno($latest);
						$_POST["partner"] = $latest['u_id'];
					} else {
						echo 'Právě s nikým netančím';
						$_POST["partner"] = "none";
					}
					if(User::getUserPohlavi() == "m") {
						$partners = DBUser::getUsersByPohlavi("f");
					} else {
						$partners = DBUser::getUsersByPohlavi("m");
					}
					
					echo '<form method="POST" action="' . $_SERVER['REQUEST_URI'] . '">';
					echoUsers("partner", $partners);
					echo '<button type="submit" name="action" value="confirm">Požádat o partnerství</button>';
					if($gotPartner)
						echo '<button type="submit" name="action" value="dumpthem">Rozejít se</button>';
					echo '</form>';
					echo '<a href=""/member/profil/par>Zpět</a>';
					return;
				} else {
					if(getPostField('action') == 'dumpthem' && !empty($latest) && $latest['u_id'] > 0) {
							DBPary::noPartner(User::getUserID());
							DBPary::noPartner($latest['u_id']);
							notice('A zase sám a sám...');
							include("files/Member/Profil/DisplayMain.inc");
							return;
					}
					if(getPostField('partner') == $latest['u_id'] ||
							(getPostField('partner') == "none" && $latest['u_id'] == '0')) {
						notice('Nic se nezměnilo.');
						include("files/Member/Profil/DisplayMain.inc");
						return;
					}
					if(getPostField("partner") == "none") {
						DBPary::noPartner(User::getUserID());
						DBPary::noPartner($latest['u_id']);
						notice('A zase sám a sám...');
						include("files/Member/Profil/DisplayMain.inc");
						return;
					} else {
						if(User::getUserPohlavi() == "m") {
							DBPary::newPartnerRequest(User::getUserID(),
								User::getUserID(), getPostField("partner"));
						} else {
							DBPary::newPartnerRequest(User::getUserID(),
								getPostField("partner"), User::getUserID());
						}
						notice("Žádost o partnerství odeslána");
						include("files/Member/Profil/DisplayMain.inc");
						return;
					}
				}
				break;
			
			case 'zadost':
				if(empty($_POST) || !$_POST['action']) {
					include("files/Member/Profil/DisplayMain.inc");
					return;
				} else {
					switch($_POST['action']) {
						case 'accept':
							$requests = DBPary::getPartnerRequestsForMe(User::getUserID());
							foreach($requests as $req) {
								if($req['pn_id'] == getPostField('id')) {
									DBPary::acceptPartnerRequest(getPostField('id'));
									
									DisplayPary::viewPartnerRequests(
										DBPary::getPartnerRequestsForMe(User::getUserID()),
										DBPary::getPartnerRequestsByMe(User::getUserID()));
									
									notice('Žádost přijata');
									include("files/Member/Profil/DisplayMain.inc");
									return;
								}
							}
							notice('Žádná taková žádost tu není');
							include("files/Member/Profil/DisplayMain.inc");
							return;
						
						case 'refuse':
							$requests = DBPary::getPartnerRequestsForMe(User::getUserID());
							foreach($requests as $req) {
								if($req['pn_id'] == getPostField('id')) {
									DBPary::deletePartnerRequest(getPostField('id'));
									
									DisplayPary::viewPartnerRequests(
										DBPary::getPartnerRequestsForMe(User::getUserID()),
										DBPary::getPartnerRequestsByMe(User::getUserID()));
									
									notice('Žádost zamítnuta');
									include("files/Member/Profil/DisplayMain.inc");
									return;
								}
							}
							notice('Žádná taková žádost tu není');
							include("files/Member/Profil/DisplayMain.inc");
							return;
						
						case 'cancel':
							$requests = DBPary::getPartnerRequestsByMe(User::getUserID());
							foreach($requests as $req) {
								if($req['pn_id'] == getPostField('id')) {
									DBPary::deletePartnerRequest(getPostField('id'));
									
									DisplayPary::viewPartnerRequests(
										DBPary::getPartnerRequestsForMe(User::getUserID()),
										DBPary::getPartnerRequestsByMe(User::getUserID()));
									
									notice('Žádost zrušena');
									include("files/Member/Profil/DisplayMain.inc");
									return;
								}
							}
							notice('Žádná taková žádost tu není');
							include("files/Member/Profil/DisplayMain.inc");
							return;
					}
				}
				break;
		}
		break;
}
?>