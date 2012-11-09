<?php
$error = false;

global $isConfirmation;
$isConfirmation = post("nadpis") || post("od") ||post('kat');

if(Request::getAction() == 'edit')
	$header = "Upravit inzerát";
elseif(Request::getAction() == 'add')
	$header = "Přidat inzerát";
else
	$header = "Neznámá akce";
header_minor($header);
?>
<form action="<?php echo $_SERVER['REQUEST_URI']; ?>" method="POST">
<table>
	<tr>
		<td>Nadpis (*): </td>
		<td><input type="text" name="nadpis" value="<?php echo post("nadpis"); ?>" /></td>
		<td>Max. 255 znaků<?php
			$error = checkPostFieldLength(1, 255, "nadpis") || $error;
		?></td>
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
		<td><?php
			$error = checkPostField("/1|2|3|4/i", "kat") || $error;
		?></td>
	</tr><?php if(User::isLogged()) { ?><tr>
		<td>Viditelný: </td>
		<td><label><?php echo getCheckbox('visible', 'visible');?></label></td>
		<td></td>
	</tr><?php } else { //TODO: Inzerce foto ?><tr>
		<td>Jméno (*): </td>
		<td><input type="text" name="jmeno" value="<?php echo post("jmeno"); ?>" /></td>
		<td>Max. 40 znaků<?php
			$error = checkPostFieldLength(1, 40, "jmeno") || $error;
		?></td>
	</tr><tr>
		<td>Příjmení (*): </td>
		<td><input type="text" name="prijmeni" value="<?php echo post("prijmeni"); ?>" /></td>
		<td>Max. 40 znaků<?php
			$error = checkPostFieldLength(1, 40, "prijmeni") || $error;
		?></td>
	</tr><?php if(empty($_SESSION['inzerce_' . Request::getID()])) { ?><tr>
		<td>Heslo (*): </td>
		<td><input type="text" name="pass" value="<?php echo post("pass"); ?>" /></td>
		<td>Max. 20 znaků - dá se použít pro opětovný přístup k inzerátu<?php
			$error = checkPostFieldLength(1, 20, "pass") || $error;
		?></td>
	</tr><?php }} ?><tr>
		<td>Od: (*)</td>
		<td><?php echo Helper::get()->date()->name('od')->textBox();?></td>
		<td><?php $error = checkDateString(Helper::get()->date(false)->getPost()) || $error;?></td>
	</tr><tr>
		<td>Do: </td>
		<td><?php echo Helper::get()->date()->name('do')->textBox();?></td>
		<td>(pokud je prázdný, počítá se jako "Od")
		<?php $do = Helper::get()->date(false)->getPost();
			$error = ($do ? checkDateString($do) : false) || $error;
		?></td>
	</tr><tr>
		<?php if(Request::getAction() == 'add') { ?>
		<td><button type="submit" name="action" value="add_confirm">Přidat</button></td>
		<?php }	else {?>
		<td><button type="submit" name="action" value="edit_confirm">Upravit</button></td>
		<?php } ?>
		<td></td>
		<td>Pole označená (*) jsou povinná</td>
	</tr>
</table>
</form>
<a href="<?php echo Request::getReferer();?>">Zpět</a>