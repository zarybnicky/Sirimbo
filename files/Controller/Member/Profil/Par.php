<?php
include_once('files/Controller/Member/Profil.php');
class Controller_Member_Profil_Par extends Controller_Member_Profil {
	function view($id = null) {
		notice(View::getRedirectMessage());
		
		DisplayPary::viewPartnerRequests(DBPary::getPartnerRequestsForMe(User::getUserID()),
		DBPary::getPartnerRequestsByMe(User::getUserID()));
		
		$latest = DBPary::getLatestPartner(User::getUserID(), User::getUserPohlavi());
		
		if(!empty($latest) && $latest['u_id']) {
			echo "Právě tančím s: ";
			echoFullJmeno($latest);
			header_minor('Třídy a body:');
			echo 
				'Standardní tance:<br />',
				'Třída: <span style="font-weight:bolder;">', $latest['p_stt_trida'],
				'</span>, body: ', $latest['p_stt_body'],
				', finále: ', $latest['p_stt_finale'], '<br />',
				'Latinsko-americké tance:<br />',
				'Třída: <span style="font-weight:bolder;">', $latest['p_lat_trida'],
				'</span>, body: ', $latest['p_lat_body'],
				', finále: ', $latest['p_lat_finale'], '<br /><br/>',
				'V "Olympáckém žebříčku" máme ', $latest['p_hodnoceni'], ' bodů.<br />';
			echo '<a href="/member/profil/par/body">Změnit třídu a body</a>';
		} else {
			echo 'Právě s nikým netančím.<br/>';
		}
		echo '<a href="/member/profil/par/partner">Změnit partnera</a>';
		echo '<a href="/member/profil">Zpět</a>';
		return;
	}
	function body($id = null) {
		DisplayPary::viewPartnerRequests(DBPary::getPartnerRequestsForMe(User::getUserID()),
			DBPary::getPartnerRequestsByMe(User::getUserID()));
		
		if(empty($_POST)) {
			$par = DBPary::getSinglePar(User::getParID());
			post('stt-trida', $par['p_stt_trida']);
			post('stt-body', $par['p_stt_body']);
			post('stt-finale', $par['p_stt_finale']);
			post('lat-trida', $par['p_lat_trida']);
			post('lat-body', $par['p_lat_body']);
			post('lat-finale', $par['p_lat_finale']);
			
			include('files/Member/Profil/FormClass.inc');
			return;	
		}
		$f = new Form();
		$f->checkInArray(post('stt-trida'), array('Z', 'H', 'D', 'C', 'B', 'A', 'M'),
			'Neplatná strndartní třída', 'stt-trida');
		$f->checkInArray(post('lat-trida'), array('Z', 'H', 'D', 'C', 'B', 'A', 'M'),
			'Neplatná latinská třída', 'lat-trida');
		$f->checkNumberBetween(post('stt-body'), 0, 1000, 'Špatný počet standartních bodů', 'stt-body');
		$f->checkNumberBetween(post('lat-body'), 0, 1000, 'Špatný počet latinských bodů', 'lat-body');
		$f->checkNumberBetween(post('stt-finale'), 0, 10, 'Špatný počet standartních finálí', 'stt-finale');
		$f->checkNumberBetween(post('lat-finale'), 0, 10, 'Špatný počet latinských finálí', 'lat-finale');
		
		if(!$f->isValid()) {
			include('files/Member/Profil/FormClass.inc');
			return;
		}
		
		$stt_base = (post('stt-body') + 40 * post('stt-finale')) * constant('AMEND_' . post('stt-trida'));
		$lat_base = (post('stt-body') + 40 * post('lat-finale')) * constant('AMEND_' . post('lat-trida'));
		
		$hodnoceni = $stt_base + $lat_base +
			constant('BONUS_' . post('stt-trida')) + constant('BONUS_' . post('lat-trida'));
		
		DBPary::editTridaBody(User::getParID(),
			post('stt-trida'), post('stt-body'), post('stt-finale'),
			post('lat-trida'), post('lat-body'), post('lat-finale'), $hodnoceni);
		
		View::redirect("/member/profil/par", "Třída a body změněny");
		return;
	}
	function partner($id = null) {
		DisplayPary::viewPartnerRequests(DBPary::getPartnerRequestsForMe(User::getUserID()),
			DBPary::getPartnerRequestsByMe(User::getUserID()));
		
		$latest = DBPary::getLatestPartner(User::getUserID(), User::getUserPohlavi());
		$gotPartner = !empty($latest) && $latest['u_id'];
		
		if(!empty($_POST)) {
			if(post('action') == 'dumpthem' && $gotPartner) {
				DBPary::noPartner(User::getUserID());
				DBPary::noPartner($latest['u_id']);
				View::redirect('/member/profil/par', 'Partnerství zrušeno');
			}
			if(post('partner') == $latest['u_id'] ||
					(post('partner') == "none" && $latest['u_id'] == '0')) {
				View::redirect('/member/profil/par', 'Nic se nezměnilo');
			}
			if(post("partner") == "none") {
				DBPary::noPartner(User::getUserID());
				DBPary::noPartner($latest['u_id']);
				View::redirect('/member/profil/par', 'Partnerství zrušeno');
			} else {
				if(User::getUserPohlavi() == "m") {
					DBPary::newPartnerRequest(User::getUserID(),
						User::getUserID(), post("partner"));
				} else {
					DBPary::newPartnerRequest(User::getUserID(),
						post("partner"), User::getUserID());
				}
				View::redirect('/member/profil/par', 'Žádost o partnerství odeslána');
			}
		}
		if($gotPartner) {
			echo "Právě tančím s: ";
			echoFullJmeno($latest);
			post("partner", $latest['u_id']);
		} else {
			echo 'Právě s nikým netančím';
			post("partner", "none");
		}
		echo '<form method="POST" action="' . Request::getURI() . '">';
		echo Helper::get()->userSelect()
			->name('partner')
			->users((User::getUserPohlavi() == "m") ? DBUser::getUsersByPohlavi("f") : DBUser::getUsersByPohlavi("m"));
		echo '<button type="submit" name="action" value="confirm">Požádat o partnerství</button>';
		if($gotPartner)
			echo '<button type="submit" name="action" value="dumpthem">Rozejít se</button>';
		echo '<a href="/member/profil/par">Zpět</a>';
		echo '</form>';
		return;
		return;
	}
	function zadost($id = null) {
		if(!post('action'))
			View::redirect('/member/profil');
		switch(post('action')) {
			case 'accept':
				$requests = DBPary::getPartnerRequestsForMe(User::getUserID());
				foreach($requests as $req) {
					if($req['pn_id'] == post('id')) {
						DBPary::acceptPartnerRequest(post('id'));
						View::redirect('/member/profil/par', 'Žádost přijata');
					}
				}
				break;
			case 'refuse':
				$requests = DBPary::getPartnerRequestsForMe(User::getUserID());
				foreach($requests as $req) {
					if($req['pn_id'] == post('id')) {
						DBPary::deletePartnerRequest(post('id'));
						View::redirect('/member/profil/par', 'Žádost zamítnuta');
					}
				}
				break;
			case 'cancel':
				$requests = DBPary::getPartnerRequestsByMe(User::getUserID());
				foreach($requests as $req) {
					if($req['pn_id'] == post('id')) {
						DBPary::deletePartnerRequest(post('id'));
						View::redirect('/member/profil/par', 'Žádost zrušena');
					}
				}
				break;
			default:
				View::redirect('/member/profil');
				break;
		}
		View::redirect('/member/profil', 'Žádná taková žádost tu není');
	}
}
?>