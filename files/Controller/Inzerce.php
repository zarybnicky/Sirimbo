<?php
class Controller_Inzerce implements Controller_Interface {
    function view($id = null) {
        View::redirect('/inzerce/posledni');
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
			include('files/Admin/Skupiny/Form.inc');
			return;
		}
		$od = Helper::get()->date()->name('od')->getPost();
		$do = Helper::get()->date()->name('do')->getPost();
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
			View::redirect("/inzerce/posledni", "Váš inzerát byl přidán");
		else
			View::redirect('/inzerce/posledni', 'Váš inzerát byl uložen a čeká na schválení administrátora');
    }
    function edit($id = null) {
		if(!$id || !($data = DBInzerce::getSingleInzerat($id)))
			View::redirect('/inzerce/posledni', 'Inzerát s takovým ID neexistuje');
        
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
        if(!$this->checkData($_POST, 'edit')) {
			include('files/Admin/Skupiny/Form.inc');
			return;
		}
		$od = Helper::get()->date()->name('od')->getPost();
		$do = Helper::get()->date()->name('do')->getPost();
		if(!$do || strcmp($od, $do) > 0)
			$do = $od;
        
		DBInzerce::editInzerat($id, post('kat'), User::getUserID(), User::getUserJmeno(),
			User::getUserPrijmeni(), post('nadpis'), post('text'), serialize(array()),
			$od, $do, '', (bool) post('visible'), '1');
		View::redirect("/inzerce/posledni", "Inzerát upraven");
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
			
			include('files/Main/Inzerce/Form.inc');
			return;
		}
        if(!$this->checkData($_POST, 'edit-unreg')) {
			include('files/Admin/Skupiny/Form.inc');
			return;
		}
		$od = Helper::get()->date()->name('od')->getPost();
		$do = Helper::get()->date()->name('do')->getPost();
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
		View::redirect("/inzerce/posledni", "Inzerát upraven");
    }
    function remove($id = null) {
		if(!$id || !($data = DBInzerce::getSingleInzerat($id)))
			View::redirect('/inzerce/posledni', 'Inzerát s takovým ID neexistuje');
		
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
        		View::redirect(Request::getURI(), 'Inzerát s takovým ID neexistuje');
        	
        	if($data['i_pass'] != $pass)
        		View::viewError(ER_AUTHORIZATION);
        	
        	session('inzerce_' . $id, $pass);
        	
        	View::redirect('/inzerce/edit-unreg/' . $id);
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