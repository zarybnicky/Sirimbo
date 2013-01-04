<?php
User::checkPermissionsError(L_ADMIN);
header_main("Správa inzerce");

notice(View::getRedirectMessage());

switch(Request::getAction()) {
	case 'add':
		if(empty($_POST)) {
			include('files/Admin/Inzerce/Form.inc');
			return;
		}
		$od = Helper::get()->date()->name('od')->getPost();
		$do = Helper::get()->date()->name('do')->getPost();
		if(!$do || strcmp($od, $do) > 0)
			$do = $od;
		
		$f = new Form();
		$f->checkLength(post('nadpis'), 1, 255, 'Špatná délka nadpisu', 'nadpis');
		$f->checkInArray(post('kat'), array(1,2,3,4), 'Neplatná kategorie', 'kat');
		$f->checkNumeric(post('reg'), 'Neplatný uživatel', 'reg');
		$f->checkDate($od, 'Neplatný formát data ("Od")', 'od');
		$f->checkDate($do, 'Neplatný formát data ("Do")', 'do');
		
		if(!$f->isValid()) {
			include('files/Admin/Inzerce/Form.inc');
			return;
		}
		$userid = post('reg');
		$user_data = DBUser::getUserData(post('reg'));
		$jmeno = post('jmeno') ? post('jmeno') : $user_data['u_jmeno'];
		$prijmeni = post('prijmeni') ? post('prijmeni') : $user_data['u_prijmeni'];
		
		DBInzerce::addInzerat(post('kat'), $userid, $jmeno, $prijmeni,
			post('nadpis'), post('text'), serialize(array()), $od, $do,
			'', (bool) post('visible'), '1');
		View::redirect('/admin/inzerce', 'Inzerát přidán');
		return;
	
	case 'edit':
		$id = Request::getID();
		if(!$id || !($data = DBInzerce::getSingleInzerat($id)))
			View::redirect('/admin/inzerce', 'Inzerát s takovým ID neexistuje');
		
		if(empty($_POST)) {
			post('kat', $data['i_kat']);
			post('reg', $data['i_reg']);
			post('jmeno', $data['i_jmeno']);
			post('prijmeni', $data['i_prijmeni']);
			post('nadpis', $data['i_nadpis']);
			post('text', $data['i_text']);
			post('od', $data['i_od']);
			post('do', $data['i_do']);
			post('pass', $data['i_pass']);
			post('visible', $data['i_visible']);
			post('confirmed', $data['i_confirmed']);
			
			include('files/Admin/Inzerce/Form.inc');
			return;
		}
		$od = Helper::get()->date()->name('od')->getPost();
		$do = Helper::get()->date()->name('do')->getPost();
		if(!$do || strcmp($od, $do) > 0)
			$do = $od;
			
		$f = new Form();
		$f->checkLength(post('nadpis'), 1, 255, 'Špatná délka nadpisu', 'nadpis');
		$f->checkInArray(post('kat'), array(1,2,3,4), 'Neplatná kategorie', 'kat');
		$f->checkNumeric(post('reg'), 'Neplatný uživatel', 'reg');
		$f->checkDate($od, 'Neplatný formát data ("Od")', 'od');
		$f->checkDate($do, 'Neplatný formát data ("Do")', 'do');
		
		if(!$f->isValid()) {
			include('files/Admin/Inzerce/Form.inc');
			return;
		}
		
		$userid = post('reg');
		$user_data = DBUser::getUserData(post('reg'));
		$jmeno = $user_data['u_jmeno'];
		$prijmeni = $user_data['u_prijmeni'];
		
		DBInzerce::editInzerat($id, post('kat'), $userid, $jmeno, $prijmeni,
			post('nadpis'), post('text'), serialize(array()), $od, $do,
			$data['i_pass'], (bool) post('visible'), '1');
		View::redirect("/admin/inzerce", "Inzerát upraven");
		return;
	case 'new':
		if(empty($_POST)) {
			include('files/Admin/Inzerce/DisplayNew.inc');
			return;
		}
		if(post('action') == 'confirm') {
			if(is_array(post('inzerce'))) {
				foreach(post('inzerce') as $id) {
					$visible = post($id . '-visible');
					DBInzerce::confirmInzerat($id, (bool) $visible);
				}
			}
			View::redirect('/admin/inzerce', 'Inzeráty potvrzeny');
			return;
		} elseif(post('action') == 'remove') {
			echo '<form action="/admin/inzerce" method="POST">';
			echo 'Opravdu chcete odstranit inzeráty:<br/><br/>';
			foreach(post('inzerce') as $id) {
				$data = DBInzerce::getSingleInzerat($id);
				echo "\t", $data['i_nadpis'];
				echo '<br />';
				echo '<input type="hidden" name=inzerce[]" value="', $id, '" />';
			}
			echo '<br/>';
			echo '<button type="submit" name="action" value="remove_confirm">Odstranit</button>';
			echo '</form>';
			echo '<a href="/admin/inzerce">Zpět</a>';
			return;
		}
		break;
}

if(empty($_POST)) {
	include("files/Admin/Inzerce/Display.inc");
	return;
}

switch(post("action")) {
	case 'edit':
		$inzerce = post('inzerce');
		if($inzerce[0]) {
			header('Location: /admin/inzerce/edit/' . $inzerce[0]);
			return;
		}
		break;

	case 'remove':
		if(is_array(post('inzerce'))) {
			echo '<form action="/admin/inzerce" method="post">';
			echo 'Opravdu chcete odstranit inzeráty:<br/><br/>';
			foreach(post('inzerce') as $id) {
				$data = DBInzerce::getSingleInzerat($id);
				echo "\t", $data['i_nadpis'];
				echo '<br />';
				echo '<input type="hidden" name=inzerce[]" value="', $id, '" />';
			}
			echo '<br/>';
			echo '<button type="submit" name="action" value="remove_confirm">Odstranit</button>';
			echo '</form>';
			echo '<a href="/admin/inzerce">Zpět</a>';
			return;
		}
		break;

	case 'remove_confirm':
		if(is_array(post("inzerce"))) {
			foreach(post("inzerce") as $item) {
				$data = DBInzerce::getSingleInzerat($item);
				DBInzerce::removeInzerat($item);
			}
			notice("Inzeráty odebrány");
			include("files/Admin/Inzerce/Display.inc");
			return;
		}
		break;
}
include("files/Admin/Inzerce/Display.inc");
return;
?>