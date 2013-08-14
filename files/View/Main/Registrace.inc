<?php header_main('Registrace'); ?>
<?php notice($this->redirect()->getRedirectMessage());?>

<form action="" method="POST">
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
		<td><?php echo $this->date('narozeni')->textBox();?></td>
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