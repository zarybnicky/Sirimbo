<?php
header_main('Inzerce');
if(Request::getAction() == 'add')
	header_minor("Přidat inzerát");
else
	header_minor('Upravit inzerát');

notice(View::getRedirectMessage());
if(is_object($f))
	notice(implode('<br/>', $f->getMessages()));
?>
<form action="<?php echo Request::getURI()?>" method="POST">
<table>
	<tr>
		<td>Nadpis (*): </td>
		<td><input type="text" name="nadpis" value="<?php echo post("nadpis"); ?>" /></td>
		<td>Max. 255 znaků</td>
	</tr><tr>
		<td>Text: </td>
		<td><textarea rows="15" cols="40" name="text"><?php echo post("text"); ?></textarea></td>
		<td></td>
	</tr><tr>
		<td>Kategorie (*): </td>
		<td>
			<label><?php echo getRadio('kat', 1);?>Prodám</label>&nbsp;
			<label><?php echo getRadio('kat', 2);?>Koupím</label><br/>
			<label><?php echo getRadio('kat', 3);?>Hledám partnera</label>&nbsp;
			<label><?php echo getRadio('kat', 4);?>Hledám partnerku</label>
		</td>
		<td></td>
	</tr><?php if(User::isLogged()) { ?><tr>
		<td>Viditelný: </td>
		<td><label><?php echo getCheckbox('visible', 'visible');?></label></td>
		<td></td>
	</tr><?php } else { //TODO: Inzerce foto ?><tr>
		<td>Jméno (*): </td>
		<td><input type="text" name="jmeno" value="<?php echo post("jmeno"); ?>" /></td>
		<td>Max. 40 znaků</td>
	</tr><tr>
		<td>Příjmení (*): </td>
		<td><input type="text" name="prijmeni" value="<?php echo post("prijmeni"); ?>" /></td>
		<td>Max. 40 znaků</td>
	</tr><?php if(empty($_SESSION['inzerce_' . Request::getID()])) { ?><tr>
		<td>Heslo (*): </td>
		<td><input type="text" name="pass" value="<?php echo post("pass"); ?>" /></td>
		<td>Max. 20 znaků - dá se použít pro opětovný přístup k inzerátu</td>
	</tr><?php }} ?><tr>
		<td>Od: (*)</td>
		<td><?php echo Helper::get()->date()->name('od')->textBox();?></td>
		<td></td>
	</tr><tr>
		<td>Do: </td>
		<td><?php echo Helper::get()->date()->name('do')->textBox();?></td>
		<td>(pokud je prázdný, počítá se jako "Od")</td>
	</tr><tr>
		<td>
			<button type="submit" name="action">
			<?php echo (Request::getAction() == 'add' ? 'Přidat' : 'Upravit'); ?>
			</button>
		</td>
		<td>Pole označená (*) jsou povinná</td>
		<td></td>
	</tr>
</table>
</form>
<a href="<?php echo Request::getReferer();?>">Zpět</a>