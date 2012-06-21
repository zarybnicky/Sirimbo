<?php
if(post('action') == 'gen_pass') {
	if($data = DBUser::getUserDataByName(post('name'))) {
		$base = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
		$password = substr(str_shuffle(str_repeat($base, 5)), 0, 8);
		$password_crypt = User::Crypt($password);
		
		DBUser::setPassword($data['u_id'], $password_crypt);
		
		if(DBUser::checkUser($data['u_login'], $password_crypt)) {
			Mailer::new_password($data['u_email'], $password);
			View::redirect('/home', 'Povedlo se, za chvíli byste měli dostat e-mail s novým heslem.');
		} else {
			notice('Něco se nepovedlo.');
		}
	} else {
		notice("Zadali jste neexistující uživatelské jméno.");
	}
}
?>

<p style="text-align:center;">Pokud jste zapomněli heslo, na váš e-mail Vám pošleme nové.</p>

<form action="<?php echo $_SERVER['REQUEST_URI'];?>" method="post">
<table style="margin:0 auto;">
<tr>
	<td>Uživatelské jméno:</td>
	<td><input type="text" name="name" /></td>
</tr>
<tr>
	<td></td>
	<td><button type="submit" name="action" value="gen_pass">Generovat heslo</button></td>
</tr>
</table>
</form>
