<?php
class Controller_Registrace implements Controller_Interface {
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
        header_main('Registrace');
?>
<form action="<?php echo Request::getURL(); ?>" method="POST">
<table>
	<tr>
		<td>Uživatel (*): </td>
		<td><input type="text" name="username" value="<?php echo post("username"); ?>" /></td>
		<td>Pouze písmena bez diakritiky, číslice a podtržítka, 3 - 20 znaků</td>
	</tr><tr>
		<td>Heslo (*): </td>
		<td><input type="password" name="pass" value="<?php echo post("pass"); ?>"/></td>
		<td>Pouze písmena bez diakritiky, číslice a podtržítka, 6 - 32 znaků</td>
	</tr><tr>
		<td>Jméno (*): </td>
		<td><input type="text" name="jmeno" value="<?php echo post("jmeno"); ?>" /></td>
		<td>Max. 40 znaků</td>
	</tr><tr>
		<td>Příjmení (*): </td>
		<td><input type="text" name="prijmeni" value="<?php echo post("prijmeni"); ?>" /></td>
		<td>Max. 40 znaků</td>
	</tr><tr>
		<td>Pohlaví (*): </td>
		<td>
			<label><?php echo getRadio('pohlavi', 'm');?>Muž</label>&nbsp;
			<label><?php echo getRadio('pohlavi', 'f');?>Žena</label>
		</td>
	</tr><tr>
		<td>E-mail (*): </td>
		<td><input type="text" name="email" value="<?php echo post("email"); ?>" /></td>
		<td>Max. 50 znaků, ve formátu e-mail adresy</td>
	</tr><tr>
		<td>Telefon (*): </td>
		<td><input type="text" name="telefon" value="<?php echo post("telefon"); ?>" /></td>
		<td></td>
	</tr><tr>
		<td>Datum narození: (*)</td>
		<td><?php echo Helper::get()->date()->name('narozeni')->textBox();?></td>
		<td></td>
	</tr><tr>
		<td>Poznámky: </td>
		<td><textarea rows="15" cols="35" name="poznamky"><?php echo post("poznamky"); ?></textarea></td>
		<td></td>
	</tr><tr>
		<td><button type="submit" name="action" value="register">Registrovat</button></td>
		<td>Pole označená (*) jsou povinná</td>
		<td></td>
	</tr>
</table>
</form>
<?php
    }
}
?>