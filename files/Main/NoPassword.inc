<?php
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
?>

<p style="text-align:center;">Pokud jste zapomněli heslo, pošleme Vám nové na e-mail, který
jste zadali při registraci</p>

<form action="<?php echo $_SERVER['REQUEST_URI'];?>" method="post">
<button type="submit" name="action" value="enter" style="display:none;">Enter</button>
<table style="margin:0 auto;">
<tr>
	<td>Uživatelské jméno:</td>
	<td><input type="text" name="name" /></td>
</tr>
<tr>
	<td>Email:</td>
	<td><input type="text" name="email" /></td>
</tr>
<tr>
	<td></td>
	<td><button type="submit" name="action" value="gen_pass">Generovat heslo</button></td>
</tr>
</table>
</form>
