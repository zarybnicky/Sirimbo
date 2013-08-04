<?php
class Controller_Nopassword extends Controller_Abstract {
	function sidebar() {
		
	}
    function view($id = null) {
        if(post('action') == 'gen_pass' || post('action') == 'enter') {
        	post('name', strtolower(post('name')));
        	
        	if($data = DBUser::getUserDataByNameEmail(post('name'), post('email'))) {
        		$base = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
        		$password = substr(str_shuffle(str_repeat($base, 5)), 0, 8);
        		$password_crypt = User::Crypt($password);
        		
        		DBUser::setPassword($data['u_id'], $password_crypt);
        		
        		if(DBUser::checkUser($data['u_login'], $password_crypt)) {
        			Mailer::new_password($data['u_email'], $password);
        			View::redirect('/home', 'Povedlo se, za chvíli byste měli dostat e-mail s novým heslem.<br/>' .
        				'V případě problémů prosím kontaktujte administrátora.');
        		} else {
        			notice('Něco se nepovedlo, prosím kontaktujte administrátora.');
        		}
        	} else {
        		notice("Špatná kombinace přihlašovacího jména a emailu");
        	}
        }
		include('files/Main/Nopassword.inc');
    }
}
?>