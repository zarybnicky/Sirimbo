<?php
include_once('files/Controller/Member/Profil.php');
class Controller_Member_Profil_Inzerce extends Controller_Member_Profil {
	function view($id = null) {
		echo '<a href="/member/profil/inzerce/add">Přidat</a><br/>';
		$result = DisplayInzerce::viewInzerce(false, true, INZERCE_ALL, 0, User::getUserID());
		if(!$result) {
			notice('Žádné inzeráty');
		}
		echo '<a href="/member/profil">Zpět</a>';
	}
	function add($id = null) {
		if(empty($_POST) || is_object($this->checkData($_POST, 'edit'))) {
			$this->render('files/Main/Inzerce/Form.inc');
			return;
		}
		$od = $this->date()->name('od')->getPost();
		$do = $this->date()->name('do')->getPost();
		if(!$do || strcmp($od, $do) > 0)
			$do = $od;

		DBInzerce::addInzerat(post('kat'), User::getUserID(), User::getUserJmeno(),
			User::getUserPrijmeni(), post('nadpis'), post('text'), serialize(array()), $od, $do,
			(bool) post('visible'), '1');
		$this->redirect("/member/profil/inzerce", "Váš inzerát byl přidán");
	}
	function edit($id = null) {
		if(!$id || !($data = DBInzerce::getSingleInzerat($id)))
			$this->redirect('/member/profil/inzerce', 'Inzerát s takovým ID neexistuje');
		
		if(Permissions::check('inzerce', P_MEMBER) && $data['i_reg'] != User::getUserID())
			throw new Exception("Máte nedostatečnou autorizaci pro tuto akci!");
		
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
		if(is_object($this->checkData($_POST, 'edit'))) {
			$this->render('files/Main/Inzerce/Form.inc');
			return;
		}
		
		$od = $this->date()->name('od')->getPost();
		$do = $this->date()->name('do')->getPost();
		if(!$do || strcmp($od, $do) > 0)
			$do = $od;
		
		DBInzerce::editInzerat($id, post('kat'), User::getUserID(), User::getUserJmeno(),
			User::getUserPrijmeni(), post('nadpis'), post('text'), serialize(array()),
			$od, $do, (bool) post('visible'), '1');
		$this->redirect("/member/profil/inzerce", "Inzerát upraven");
	}
	function remove($id = null) {
		if(!$id || !($data = DBInzerce::getSingleInzerat($id)))
			$this->redirect('/member/profil/inzerce', 'Inzerát s takovým ID neexistuje');
		
		if(Permissions::check('inzerce', P_MEMBER) && $data['i_reg'] != User::getUserID())
			throw new Exception("Máte nedostatečnou autorizaci pro tuto akci!");
		
		if(empty($_POST)) {
			echo '<form action="', $_SERVER['REQUEST_URI'], '" method="post">';
			echo 'Opravdu chcete odstranit inzerát "';
			$data = DBInzerce::getSingleInzerat($id);
			echo $data['i_nadpis'], '"?<br/>';
			echo '<input type="hidden" name="id" value="', $id, '" />';
			echo '<button type="submit" name="action" value="confirm">Odstranit</button>';
			echo '<a href="/member/profil/inzerce">Zpět</a>';
			echo '</form>';
			return;
		} else {
			DBInzerce::removeInzerat($id);
			
			$this->redirect("/member/profil/inzerce", "Inzerát odebrán");
			return;
		}
	}
	private function checkData($data, $action = 'add') {
		$od = $this->date()->name('od')->getPost();
		$do = $this->date()->name('do')->getPost();
		if(!$do || strcmp($od, $do) > 0)
			$do = $od;
		
		$f = new Form();
		$f->checkLength(post('nadpis'), 1, 40, 'Zadejte prosím nadpis správně', 'nadpis');
		$f->checkInArray(post('kat'), array(1,2,3,4), 'Špatná kategorie', 'kat');
		$f->checkDate($od, 'Zadejte prosím platné datum ("Od")', 'od');
		$f->checkDate($do, 'Zadejte prosím platné datum ("Do")', 'do');
		
		return $f->isValid() ? true : $f;
	}
}
?>