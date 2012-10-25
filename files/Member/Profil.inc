<?php
User::checkPermissionsError(L_HOST);
header_main("Profil");

notice(View::getRedirectMessage());

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
			
			post("login", User::getUserName());
			post("level", $data["u_level"]);
			switch($data["u_level"]) {
				case L_USER:
					post("level", "user");	break;
				case L_EDITOR:
					post("level", "editor");	break;
				case L_TRENER:
					post("level", "trener");	break;
				case L_ADMIN:
					post("level", "admin");	break;
				case L_SADMIN:
					post("level", "sadmin");	break;
				default:
					post("level", "user");	break;
			}
			post("lock", $data["u_lock"]);
			post("jmeno", $data["u_jmeno"]);
			post("prijmeni", $data["u_prijmeni"]);
			post("pohlavi", $data["u_pohlavi"]);
			post("email", $data["u_email"]);
			post("telefon", $data["u_telefon"]);
			list($year, $month, $day) = explode('-', $data['u_narozeni']);
			post('year', $year);
			post('month', $month);
			post('day', $day);
			post("poznamky", $data["u_poznamky"]);
			
			include("files/Member/Profil/FormPerson.inc");
			return;
		} else {
			$error = false;
			ob_start();
			include('files/Member/Profil/FormPerson.inc');
			$html = ob_get_clean();
			
			if(!$error) {
				$narozeni = post('year') . '-' . post('month') . '-' . post('day');
				
				DBUser::setUserData(User::getUserID(), post('jmeno'), post('prijmeni'),
					post('pohlavi'), post('email'), post('telefon'), $narozeni,
					post('poznamky'), $data['u_level'], $data['u_skupina'],
					$data['u_dancer'], $data['u_lock'], $data['u_ban'], $data['u_system']);
				
				View::redirect('/member/profil', 'Upraveno');
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
			$oldpass_check = DBUser::checkUser(User::getUserName(), User::Crypt(post('oldpass')));
			$newpass_check = post('newpass') == post('newpass_confirm');
			
			ob_start();
			include('files/Member/Profil/FormPass.inc');	
			$html = ob_get_clean();
			
			if(!$error && $oldpass_check && $newpass_check) {
				DBUser::setPassword(User::getUserID(), User::Crypt(post('newpass')));
				View::redirect('/member/profil', 'Heslo změněno');
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
					post('stt-trida', $vysledky['p_stt_trida']);
					post('stt-body', $vysledky['p_stt_body']);
					post('stt-finale', $vysledky['p_stt_finale']);
					post('lat-trida', $vysledky['p_lat_trida']);
					post('lat-body', $vysledky['p_lat_body']);
					post('lat-finale', $vysledky['p_lat_finale']);
					
					include('files/Member/Profil/FormClass.inc');
					return;	
				} else {
					$stt_trida = post('stt-trida');
					$lat_trida = post('lat-trida');
					
					$post_stt_body = post('stt-body');
					$stt_body = ($post_stt_body && $post_stt_body < 1000 && $post_stt_body > 0) ?
						$post_stt_body : 0;
						
					$post_stt_finale = post('stt-finale');
					$stt_finale = ($post_stt_finale && $post_stt_finale < 1000 && $post_stt_finale > 0) ?
						$post_stt_finale : 0;
					
					$post_lat_body = post('lat-body');
					$lat_body = ($post_lat_body && $post_lat_body < 1000 && $post_lat_body > 0) ?
						$post_lat_body : 0;
						
					$post_lat_finale = post('lat-finale');
					$lat_finale = ($post_lat_finale && $post_lat_finale < 1000 && $post_lat_finale > 0) ?
						$post_lat_finale : 0;
					
					$stt_base = ($stt_body + 40 * $stt_finale) * constant('AMEND_' . $stt_trida);
					$lat_base = ($lat_body + 40 * $lat_finale) * constant('AMEND_' . $lat_trida);
					
					$hodnoceni = $stt_base + $lat_base +
						constant('BONUS_' . $stt_trida) + constant('BONUS_' . $lat_trida);
					
					DBPary::editTridaBody(User::getParID(), $stt_trida, $stt_body,
						$stt_finale, $lat_trida, $lat_body, $lat_finale, $hodnoceni);
					
					View::redirect("/member/profil/par", "Třída a body změněny");
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
						post("partner", $latest['u_id']);
					} else {
						echo 'Právě s nikým netančím';
						post("partner", "none");
					}
					if(User::getUserPohlavi() == "m") {
						$partners = DBUser::getUsersByPohlavi("f");
					} else {
						$partners = DBUser::getUsersByPohlavi("m");
					}
					$userselect = new UserSelectHelper();
					
					echo '<form method="POST" action="' . $_SERVER['REQUEST_URI'] . '">';
					echo $userselect->name('partner')->users($partners)->tmpSwitch(FALSE);
					echo '<button type="submit" name="action" value="confirm">Požádat o partnerství</button>';
					if($gotPartner)
						echo '<button type="submit" name="action" value="dumpthem">Rozejít se</button>';
					echo '</form>';
					echo '<a href="/member/profil/par">Zpět</a>';
					return;
				} else {
					if(post('action') == 'dumpthem' && !empty($latest) && $latest['u_id'] > 0) {
							DBPary::noPartner(User::getUserID());
							DBPary::noPartner($latest['u_id']);
							notice('A zase sám a sám...');
							include("files/Member/Profil/DisplayMain.inc");
							return;
					}
					if(post('partner') == $latest['u_id'] ||
							(post('partner') == "none" && $latest['u_id'] == '0')) {
						notice('Nic se nezměnilo.');
						include("files/Member/Profil/DisplayMain.inc");
						return;
					}
					if(post("partner") == "none") {
						DBPary::noPartner(User::getUserID());
						DBPary::noPartner($latest['u_id']);
						notice('A zase sám a sám...');
						include("files/Member/Profil/DisplayMain.inc");
						return;
					} else {
						if(User::getUserPohlavi() == "m") {
							DBPary::newPartnerRequest(User::getUserID(),
								User::getUserID(), post("partner"));
						} else {
							DBPary::newPartnerRequest(User::getUserID(),
								post("partner"), User::getUserID());
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
								if($req['pn_id'] == post('id')) {
									DBPary::acceptPartnerRequest(post('id'));
									
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
								if($req['pn_id'] == post('id')) {
									DBPary::deletePartnerRequest(post('id'));
									
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
								if($req['pn_id'] == post('id')) {
									DBPary::deletePartnerRequest(post('id'));
									
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
	case 'inzerce':
		header_minor('Moje inzerce');
		
		if(!isset($action[1])) {
			echo '<a href="/member/profil/inzerce/add">Přidat</a><br/>';
			$result = DisplayInzerce::viewInzerce(false, true, INZERCE_ALL, 0, User::getUserID());
			if(!$result) {
				notice('Žádné inzeráty');
			}
			echo '<a href="/member/profil">Zpět</a>';
			return;
		}
		
		switch($action[1]) {
			case 'add':
				if(empty($_POST)) {
					include('files/Main/Inzerce/Form.inc');
					return;
				} else {
					$od = post('od_year') . '-' . post('od_month') . '-' . post('od_day');
					$do = post('do_year') . '-' . post('do_month') . '-' . post('do_day');
					
					if($do == '0000-00-00' || strcmp($od, $do) > 0)
						$do = $od;
					
					$error = false;
					
					ob_start();
					include('files/Main/Inzerce/Form.inc');	
					$html = ob_get_clean();
					
					if(!$error) {
						DBInzerce::addInzerat(post('kat'), User::getUserID(), User::getUserJmeno(),
							User::getUserPrijmeni(), post('nadpis'), post('text'), serialize(array()), $od, $do,
							(bool) post('visible'), '1');
						View::redirect("/member/profil/inzerce", "Váš inzerát byl přidán");
					} else {
						echo $html;
						return;
					}
				}
				break;
			case 'edit':
				$id = isset($action[2]) ? $action[2] : '';
				
				if(!$id || !is_numeric($id) || !($data = DBInzerce::getSingleInzerat($id)))
					View::redirect('/member/profil/inzerce', 'Inzerát s takovým ID neexistuje');
				
				if(!Permissions::canEditInzerat($data['i_reg']))
					View::viewError(ER_AUTHORIZATION);
				
				if(empty($_POST)) {
					post('kat', $data['i_kat']);
					post('reg', $data['i_reg']);
					post('jmeno', $data['i_jmeno']);
					post('prijmeni', $data['i_prijmeni']);
					post('nadpis', $data['i_nadpis']);
					post('text', $data['i_text']);
					list($od_year, $od_month, $od_day) = explode('-', $data['i_od']);
					post('od_year', $od_year);
					post('od_month', $od_month);
					post('od_day', $od_day);
					list($do_year, $do_month, $do_day) = explode('-', $data['i_do']);
					post('do_year', $do_year);
					post('do_month', $do_month);
					post('do_day', $do_day);
					post('visible', $data['i_visible']);
					post('confirmed', $data['i_confirmed']);
					
					include('files/Main/Inzerce/Form.inc');
					return;
				} else {
					$od = post('od_year') . '-' . post('od_month') . '-' . post('od_day');
					$do = post('do_year') . '-' . post('do_month') . '-' . post('do_day');
					
					if($do == '0000-00-00' || strcmp($od, $do) > 0)
						$do = $od;
					
					$error = false;
					
					ob_start();
					include('files/Main/Inzerce/Form.inc');	
					$html = ob_get_clean();
					
					if(!$error) {
						DBInzerce::editInzerat($id, post('kat'), User::getUserID(), User::getUserJmeno(),
							User::getUserPrijmeni(), post('nadpis'), post('text'), serialize(array()),
							$od, $do, (bool) post('visible'), '1');
						View::redirect("/member/profil/inzerce", "Inzerát upraven");
					} else {
						echo $html;
						return;
					}
				}
				break;
			case 'remove':
				$id = isset($action[2]) ? $action[2] : '';
				
				if(!$id || !is_numeric($id) || !($data = DBInzerce::getSingleInzerat($id)))
					View::redirect('/member/profil/inzerce', 'Inzerát s takovým ID neexistuje');
				
				if(!Permissions::canEditInzerat($data['i_reg']))
					View::viewError(ER_AUTHORIZATION);
				
				if(empty($_POST)) {
					echo '<form action="', $_SERVER['REQUEST_URI'], '" method="post">';
					echo 'Opravdu chcete odstranit inzerát "';
					$data = DBInzerce::getSingleInzerat($id);
					echo $data['i_nadpis'];
					echo '"?<br/>';
					echo '<input type="hidden" name="id" value="', $id, '" />';
					echo '<button type="submit" name="action" value="remove_confirm">Odstranit</button>';
					echo '<a href="/member/profil/inzerce">Zpět</a>';
					echo '</form>';
					return;
				} else {
					DBInzerce::removeInzerat($id);
					
					View::redirect("/member/profil/inzerce", "Inzerát odebrán");
					return;
				}
				break;
		}
		break;
}
?>