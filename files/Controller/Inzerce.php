<?php
class Controller_Inzerce extends Controller_Abstract {
	function view($id = null) {
		$this->redirect('/inzerce/posledni');
	}
	function sidebar() {
		$s = new Sidebar();
		
		echo $s->menuHeader();
		echo $s->menuItem('Nejnovější inzeráty','/inzerce/posledni');
		echo $s->menuItem('Prodám',				'/inzerce/prodam');
		echo $s->menuItem('Koupím'	,			'/inzerce/koupim');
		echo $s->menuItem('Hledám partnera',	'/inzerce/partner');
		echo $s->menuItem('Hledám partnerku',	'/inzerce/partnerka');
		echo $s->menuItem('Nový inzerát',		'/inzerce/add');
		
		echo $s->commonItems();
	}
	function posledni($id = null) {
		$this->inzerce('Poslední inzeráty', INZERCE_ALL);
	}
	function prodam($id = null) {
		$this->inzerce('Prodám', INZERCE_PRODAM);
	}
	function koupim($id = null) {
		$this->inzerce('Koupím', INZERCE_KOUPIM);
	}
	function partner($id = null) {
		$this->inzerce('Hledám partnera', INZERCE_PARTNER);
	}
	function partnerka($id = null) {
		$this->inzerce('Hledám partnerku', INZERCE_PARTNERKA);
	}
	function add($id = null) {
		if(empty($_POST) || !$this->checkData($_POST, 'add')) {
			$this->render('files/Admin/Skupiny/Form.inc');
			return;
		}
		$od = $this->date()->name('od')->getPost();
		$do = $this->date()->name('do')->getPost();
		if(!$do || strcmp($od, $do) > 0)
			$do = $od;
		
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
			$this->redirect("/inzerce/posledni", "Váš inzerát byl přidán");
		else
			$this->redirect('/inzerce/posledni', 'Váš inzerát byl uložen a čeká na schválení administrátora');
	}
	function edit($id = null) {
		if(!$id || !($data = DBInzerce::getSingleInzerat($id)))
			$this->redirect('/inzerce/posledni', 'Inzerát s takovým ID neexistuje');
		
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
			
			$this->render('files/Main/Inzerce/Form.inc');
			return;
		}
		if(!$this->checkData($_POST, 'edit')) {
			$this->render('files/Admin/Skupiny/Form.inc');
			return;
		}
		$od = $this->date()->name('od')->getPost();
		$do = $this->date()->name('do')->getPost();
		if(!$do || strcmp($od, $do) > 0)
			$do = $od;
		
		DBInzerce::editInzerat($id, post('kat'), User::getUserID(), User::getUserJmeno(),
			User::getUserPrijmeni(), post('nadpis'), post('text'), serialize(array()),
			$od, $do, '', (bool) post('visible'), '1');
		$this->redirect("/inzerce/posledni", "Inzerát upraven");
	}
	function edit_unreg($id = null) {
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
			
			$this->render('files/Main/Inzerce/Form.inc');
			return;
		}
		if(!$this->checkData($_POST, 'edit-unreg')) {
			$this->render('files/Admin/Skupiny/Form.inc');
			return;
		}
		$od = $this->date()->name('od')->getPost();
		$do = $this->date()->name('do')->getPost();
		if(!$do || strcmp($od, $do) > 0)
			$do = $od;
		
		$jmeno = post('jmeno');
		$prijmeni = post('prijmeni');
		$pass = post('pass');
		$confirmed = '0';
		$visible = '0';
		
		DBInzerce::editInzerat($id, post('kat'), User::getUserID(), post('jmeno'),
			post('prijmeni'), post('nadpis'), post('text'), serialize(array()),
			$od, $do, $pass, '1', '1');
		unset($_SESSION['inzerce_' . $id]);
		$this->redirect("/inzerce/posledni", "Inzerát upraven");
	}
	function remove($id = null) {
		if(!$id || !($data = DBInzerce::getSingleInzerat($id)))
			$this->redirect('/inzerce/posledni', 'Inzerát s takovým ID neexistuje');
		
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
		
		$this->redirect("/inzerce/posledni", "Inzerát odebrán");
	}
	
	private function inzerce($nadpis, $type = INZERCE_ALL) {
		header_minor($nadpis);
		$result = DisplayInzerce::viewInzerce(true, true, $type);
		if(!$result)  {
			notice('Žádné inzeráty');
		}
	}
	private function checkLogin() {
		if(!empty($_POST) && post('action') == "edit_unreg") {
			$id = post('id');
			$pass = post('pass');
			
			if(!$id || !is_numeric($id) || !($data = DBInzerce::getSingleInzerat($id)))
				$this->redirect(Request::getURI(), 'Inzerát s takovým ID neexistuje');
			
			if($data['i_pass'] != $pass)
				throw new Exception("Máte nedostatečnou autorizaci pro tuto akci!");
			
			session('inzerce_' . $id, $pass);
			
			$this->redirect('/inzerce/edit-unreg/' . $id);
		}
	}
	private function checkData($data, $action = 'add') {
		$f = new Form();
		$f->checkLength(post('nadpis'), 1, 255, 'Špatná délka nadpisu', 'nadpis');
		$f->checkInArray(post('kat'), array(1,2,3,4), 'Neplatná kategorie', 'kat');
		$f->checkDate($od, 'Neplatný formát data ("Od")', 'od');
		$f->checkDate($do, 'Neplatný formát data ("Do")', 'do');
		if($action´== 'edit-unreg' || !User::isLogged()) {
			$f->checkLength(post('jmeno'), 1, 40, 'Špatná délka jména', 'jmeno');
			$f->checkLength(post('prijmeni'), 1, 40, 'Špatná délka přijmení', 'prijmeni');
			$f->checkLength(post('pass'), 1, 20, 'Špatná délka hesla', 'pass');
		}
		return $f->isValid();
	}
}