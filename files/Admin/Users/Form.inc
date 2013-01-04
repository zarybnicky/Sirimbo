<?php
if(Request::getAction() == 'add')
	header_minor("Přidat uživatele");
else
	header_minor("Upravit uživatele");

if(isset($f))
	notice(implode('<br/>', $f->getMessages()));
?>
<form action="<?php echo Request::getURI();?>" method="POST">
<table>
	<tr>
		<td>Uživatel (*): </td>
		<td><input type="text" name="login" value="<?php echo post('login'); ?>" <?php
			if(Request::getAction() == 'edit') echo 'readonly="readonly"';
		?>/></td>
		<td>Pouze písmena bez diakritiky, číslice a podtržítka, 3 - 20 znaků</td>
	</tr><tr>
		<td>Heslo (*): </td>
		<td><input type="text" name="pass" value="<?php echo post('pass'); ?>" <?php
			if(Request::getAction() == 'edit') echo 'disabled="disabled"';
		?>/></td>
		<td>Pouze písmena bez diakritiky, číslice a podtržítka, 6 - 32 znaků</td>
	</tr><tr>
		<td>Jméno: </td>
		<td><input type="text" name="jmeno" value="<?php echo post("jmeno"); ?>" /></td>
		<td>Max. 40 znaků</td>
	</tr><tr>
		<td>Příjmení: </td>
		<td><input type="text" name="prijmeni" value="<?php echo post("prijmeni"); ?>" /></td>
		<td>Max. 40 znaků</td>
	</tr><tr>
		<td>Pohlaví (*): </td>
		<td>
			<label><?php echo getRadio('pohlavi', 'm');?>Muž</label>&nbsp;
			<label><?php echo getRadio('pohlavi', 'f');?>Žena</label>
		</td>
		<td></td>
	</tr><tr>
		<td>E-mail: </td>
		<td><input type="text" name="email" value="<?php echo post("email"); ?>" /></td>
		<td>Max. 50 znaků, ve formátu e-mail adresy</td>
	</tr><tr>
		<td>Telefon: </td>
		<td><input type="text" name="telefon" value="<?php echo post("telefon"); ?>" /></td>
		<td>Pouze číslice a +, max. 13 znaků</td>
	</tr><tr>
		<td>Datum narození: (*)</td>
		<td><?php echo Helper::get()->date()->name('narozeni')->textBox(); ?></td>
		<td></td>
	</tr><tr>
		<td>Poznámky: </td>
		<td><textarea rows="15" cols="50" name="poznamky"><?php echo post("poznamky"); ?></textarea></td>
		<td></td>
	</tr><tr>
		<td>Uzamknout: </td>
		<td><label><?php echo getCheckbox('lock', 'lock');?></label></td>
		<td></td>
	</tr><tr>
		<td>Ban: </td>
		<td><label><?php echo getCheckbox('ban', 'ban');?></label></td>
		<td></td>
	</tr><tr>
		<td>Aktivní tanečník: </td>
		<td><label><?php echo getCheckbox('dancer', 'dancer');?></label></td>
		<td></td>
	</tr><tr>
		<td>Systémový uživatel: </td>
		<td><label><?php echo getCheckbox('system', 'system');?></label></td>
		<td></td>
	</tr><tr>
		<td>Oprávnění: </td>
		<td><?php
		echo Helper::get()->select()
			->post()->name('level')
			->option('host', 'Host')
			->option('user', 'Uživatel')
			->option('editor', 'Editor')
			->option('trener', 'Trenér')
			->option('admin', 'Administrátor')
			->option('admin', 'Superuživatel');
		?></td>
		<td></td>
	</tr><tr>
		<td>Skupina: </td>
		<td><?php
		$skupiny = DBSkupiny::getSkupiny();
		foreach($skupiny as $skupina) {
			echo getColorBox($skupina['us_color'], $skupina['us_color']);
			echo '<label>', getRadio('skupina', $skupina['us_id']),
				$skupina['us_popis'], '</label><br/>';
		}
		?></td>
		<td></td>
	</tr><tr>
		<td><button type="submit"><?php
			if(Request::getAction() == 'add') echo 'Přidat';
			else echo 'Upravit';
		?></button></td>
		<td>Pole označená (*) jsou povinná</td>
		<td></td>
	</tr>
</table>
</form>
<a href="/admin/users">Zpět</a>