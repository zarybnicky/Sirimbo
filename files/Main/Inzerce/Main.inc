<?php
header_main('Inzerce');

notice(View::getRedirectMessage());

if(!empty($_POST) && post('action') == "edit_unreg") {
	$id = post('id');
	$pass = post('pass');
	
	if(!$id || !is_numeric($id) || !($data = DBInzerce::getSingleInzerat($id)))
		View::redirect(Request::getURI(), 'Inzerát s takovým ID neexistuje');
	
	if($data['i_pass'] != $pass)
		View::viewError(ER_AUTHORIZATION);
	
	session('inzerce_' . $id, $pass);
	
	View::redirect('/inzerce/edit-unreg/' . $id);
}

switch(Request::getAction()) {
	case 'posledni':
		header_minor('Poslední inzeráty');
		$result = DisplayInzerce::viewInzerce(true, true, INZERCE_ALL, INZERCE_COUNT);
		if(!$result)  {
			notice('Žádné inzeráty');
		}
		return;
	case 'prodam':
		header_minor('Prodám');
		$result = DisplayInzerce::viewInzerce(true, true, INZERCE_PRODAM);
		if(!$result)  {
			notice('Žádné inzeráty');
		}
		return;
	case 'koupim':
		header_minor('Koupím');
		$result = DisplayInzerce::viewInzerce(true, true, INZERCE_KOUPIM);
		if(!$result)  {
			notice('Žádné inzeráty');
		}
		return;
	case 'partner':
		header_minor('Hledám partnera');
		$result = DisplayInzerce::viewInzerce(true, true, INZERCE_PARTNER);
		if(!$result)  {
			notice('Žádné inzeráty');
		}
		return;
	case 'partnerka':
		header_minor('Hledám partnerku');
		$result = DisplayInzerce::viewInzerce(true, true, INZERCE_PARTNERKA);
		if(!$result)  {
			notice('Žádné inzeráty');
		}
		return;
	case 'add':
		if(empty($_POST)) {
			include('files/Main/Inzerce/Form.inc');
			return;
		}
		$od = Helper::get()->date()->name('od')->getPost();
		$do = Helper::get()->date()->name('do')->getPost();
		if(!$do || strcmp($od, $do) > 0)
			$do = $od;
		
		$f = new Form();
		$f->checkLength(post('nadpis'), 1, 255, 'Špatná délka nadpisu', 'nadpis');
		$f->checkInArray(post('kat'), array(1,2,3,4), 'Neplatná kategorie', 'kat');
		$f->checkDate($od, 'Neplatný formát data ("Od")', 'od');
		$f->checkDate($do, 'Neplatný formát data ("Do")', 'do');
		if(!User::isLogged()) {
			$f->checkLength(post('jmeno'), 1, 40, 'Špatná délka jména', 'jmeno');
			$f->checkLength(post('prijmeni'), 1, 40, 'Špatná délka přijmení', 'prijmeni');
			$f->checkLength(post('pass'), 1, 20, 'Špatná délka hesla', 'pass');
		}
		if(!$f->isValid()) {
			include('files/Admin/Inzerce/Form.inc');
			return;
		}
		if(User::isLogged()) {
			$jmeno = User::getUserJmeno();
			$prijmeni = User::getUserPrijmeni();
			$pass = '';
			$confirmed = '1';
			$visible = post('visible') ? '1' : '0';
		} else {
			$jmeno = post('jmeno');
			$prijmeni = post('prijmeni');
			$pass = post('pass');
			$confirmed = '0';
			$visible = '0';
		}
		DBInzerce::addInzerat(post('kat'), User::getUserID(), $jmeno, $prijmeni,
			post('nadpis'), post('text'), serialize(array()), $od, $do, $pass, $visible,
			$confirmed);
		if(User::isLogged())
			View::redirect("/inzerce/posledni", "Váš inzerát byl přidán");
		else
			View::redirect('/inzerce/posledni', 'Váš inzerát byl uložen a čeká na schválení administrátora');
	case 'edit':
		$id = Request::getID();
		if(!$id || !($data = DBInzerce::getSingleInzerat($id)))
			View::redirect('/inzerce/posledni', 'Inzerát s takovým ID neexistuje');
		
		if(!Permissions::canEditInzerat($data['i_reg']))
			View::viewError(ER_AUTHORIZATION);
		
		if(empty($_POST)) {
			post('kat', $data['i_kat']);
			post('reg', $data['i_reg']);
			post('jmeno', $data['i_jmeno']);
			post('prijmeni', $data['i_prijmeni']);
			post('nadpis', $data['i_nadpis']);
			post('text', $data['i_text']);
			post('od', $data['i_od']);
			post('do', $data['i_do']);
			post('visible', $data['i_visible']);
			post('confirmed', $data['i_confirmed']);
			
			include('files/Main/Inzerce/Form.inc');
			return;
		}
		$od = Helper::get()->date()->name('od')->getPost();
		$do = Helper::get()->date()->name('do')->getPost();
		if(!$do || strcmp($od, $do) > 0)
			$do = $od;
		
		$f = new Form();
		$f->checkLength(post('nadpis'), 1, 255, 'Špatná délka nadpisu', 'nadpis');
		$f->checkInArray(post('kat'), array(1,2,3,4), 'Neplatná kategorie', 'kat');
		$f->checkDate($od, 'Neplatný formát data ("Od")', 'od');
		$f->checkDate($do, 'Neplatný formát data ("Do")', 'do');
		if(!$f->isValid()) {
			include('files/Admin/Inzerce/Form.inc');
			return;
		}
		DBInzerce::editInzerat($id, post('kat'), User::getUserID(), User::getUserJmeno(),
			User::getUserPrijmeni(), post('nadpis'), post('text'), serialize(array()),
			$od, $do, '', (bool) post('visible'), '1');
		View::redirect("/inzerce/posledni", "Inzerát upraven");
		return;
	case 'edit-unreg':
		$id = Request::getID();
		if(!$id || !($data = DBInzerce::getSingleInzerat($id)))
			View::redirect('/inzerce/posledni', 'Inzerát s takovým ID neexistuje');
		
		$pass = session('inzerce_' . $id);
		if($data['i_pass'] != $pass)
			View::viewError(ER_AUTHORIZATION);
		
		if(empty($_POST)) {
			post('kat', $data['i_kat']);
			post('reg', $data['i_reg']);
			post('jmeno', $data['i_jmeno']);
			post('prijmeni', $data['i_prijmeni']);
			post('nadpis', $data['i_nadpis']);
			post('text', $data['i_text']);
			post('od', $data['i_od']);
			post('do', $data['i_do']);
			post('visible', $data['i_visible']);
			post('confirmed', $data['i_confirmed']);
			
			include('files/Main/Inzerce/Form.inc');
			return;
		}
		$od = Helper::get()->date()->name('od')->getPost();
		$do = Helper::get()->date()->name('do')->getPost();
		if(!$do || strcmp($od, $do) > 0)
			$do = $od;
		
		$f = new Form();
		$f->checkLength(post('nadpis'), 1, 255, 'Špatná délka nadpisu', 'nadpis');
		$f->checkInArray(post('kat'), array(1,2,3,4), 'Neplatná kategorie', 'kat');
		$f->checkDate($od, 'Neplatný formát data ("Od")', 'od');
		$f->checkDate($do, 'Neplatný formát data ("Do")', 'do');
		$f->checkLength(post('jmeno'), 1, 40, 'Špatná délka jména', 'jmeno');
		$f->checkLength(post('prijmeni'), 1, 40, 'Špatná délka přijmení', 'prijmeni');
		$f->checkLength(post('pass'), 1, 20, 'Špatná délka hesla', 'pass');
		if(!$f->isValid()) {
			include('files/Admin/Inzerce/Form.inc');
			return;
		}
		$jmeno = post('jmeno');
		$prijmeni = post('prijmeni');
		$pass = post('pass');
		$confirmed = '0';
		$visible = '0';
		
		DBInzerce::editInzerat($id, post('kat'), User::getUserID(), post('jmeno'),
			post('prijmeni'), post('nadpis'), post('text'), serialize(array()),
			$od, $do, $pass, '1', '1');
		unset($_SESSION['inzerce_' . $id]);
		View::redirect("/inzerce/posledni", "Inzerát upraven");
		return;
	case 'remove':
		$id = Request::getID();
		if(!$id || !($data = DBInzerce::getSingleInzerat($id)))
			View::redirect('/inzerce/posledni', 'Inzerát s takovým ID neexistuje');
		
		if(!Permissions::canEditInzerat($data['i_reg']))
			View::viewError(ER_AUTHORIZATION);
		
		if(empty($_POST)) {
			$data = DBInzerce::getSingleInzerat($id);
			
			echo '<form action="', $_SERVER['REQUEST_URI'], '" method="post">';
			echo 'Opravdu chcete odstranit inzerát "', $data['i_nadpis'], '"?<br/>';
			echo '<input type="hidden" name="id" value="', $id, '" />';
			echo '<button type="submit" name="action" value="remove_confirm">Odstranit</button>';
			echo '<a href="/inzerce/posledni">Zpět</a>';
			echo '</form>';
			return;
		}
		DBInzerce::removeInzerat($id);
		
		View::redirect("/inzerce/posledni", "Inzerát odebrán");
		return;
}
?>