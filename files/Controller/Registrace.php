<?php
class Controller_Registrace extends Controller_Abstract {
	function sidebar() {
		
	}
    function view($id = null) {
        if(!empty($_POST)) {
        	$narozeni = Helper::get()->date()->name('narozeni')->getPost();
        	
        	$f = new Form();
        	$f->checkLogin(post('username'), 'Špatný formát přihlašovacího jména', 'username');
        	$f->checkPassword(post('pass'), 'Špatný formát hesla', 'pass');
        	$f->checkLength(post('jmeno'), 1, 40, 'Špatná délka jména', 'jmeno');
        	$f->checkLength(post('prijmeni'), 1, 40, 'Špatná délka přijmení', 'prijmeni');
        	$f->checkInArray(post('pohlavi'), array('m', 'f'), 'Neplatné pohlaví', 'pohlavi');
        	$f->checkEmail(post('email'), 'Neplatný formát emailu', 'email');
        	$f->checkPhone(post('telefon'), 'Neplatný formát telefoního čísla', 'telefon');
        	$f->checkDate($narozeni, 'Neplatné datum narození', 'narozeni');
        	
        	if(!$f->isValid()) {
        		notice(implode('<br/>', $f->getMessages()));
        	} else {
        		if(DBUser::getUserID(post("username"))) {
        			notice("Už tu někdo s takovým přihlašovacím jménem je :o(");
        		} else {
        			$narozeni = Helper::get()->date()->name('narozeni')->getPost();
        			
        			User::register(post("username"), post("pass"), post("jmeno"), post("prijmeni"),
        				post('pohlavi'), post("email"), post("telefon"), $narozeni, post("poznamky"));
        			View::redirect('/done');
        		}
        	}
        }
        include('files/Main/Registrace.inc');
    }
}
?>