<?php
$error = false;

global $isConfirmation;
$isConfirmation = (post("login") || post("pass"));

if(Request::getAction() == 'edit')
	$header = "Upravit uživatele";
elseif(Request::getAction() == 'add')
	$header = "Přidat uživatele";
else
	$header = "Neznámá akce";
header_minor($header);
?>
<form action="<?php echo $_SERVER['REQUEST_URI']; ?>" method="POST">
<table>
	<tr>
		<td>Uživatel (*): </td>
		<td><input type="text" name="login" value="<?php echo post('login'); ?>" <?php
			if($action[0] == 'edit') echo 'readonly="readonly"';
		?>/>
		<input type="hidden" name="id" value="<?php $id = post("id");
			if(is_numeric($id)) echo $id; else if($id) View::viewError(ER_CORRUPT_DATA); ?>"/></td>
		<td>Pouze písmena bez diakritiky, číslice a podtržítka, 3 - 20 znaků<?php
			$error = checkPostField('/^[A-Z0-9_]{3,20}$/i', 'login');
		?></td>
	</tr><tr>
		<td>Heslo (*): </td>
		<td><input type="text" name="pass" value="<?php echo post('pass'); ?>" <?php
			if($action[0] == 'edit') echo 'disabled="disabled"';
		?>/></td>
		<td>Pouze písmena bez diakritiky, číslice a podtržítka, 6 - 32 znaků<?php
			if($action[0] != 'edit' && !post('id')) {
				$error = checkPostField('/^[A-Z0-9_]{6,32}$/i', 'pass') || $error;
			}
		?></td>
	</tr><tr>
		<td>Jméno: </td>
		<td><input type="text" name="jmeno" value="<?php echo post("jmeno"); ?>" /></td>
		<td>Max. 40 znaků<?php
			$error = checkPostFieldLength(0, 40, "jmeno") || $error;
		?></td>
	</tr><tr>
		<td>Příjmení: </td>
		<td><input type="text" name="prijmeni" value="<?php echo post("prijmeni"); ?>" /></td>
		<td>Max. 40 znaků<?php
			$error = checkPostFieldLength(0, 40, "prijmeni") || $error;
		?></td>
	</tr><tr>
		<td>Pohlaví (*): </td>
		<td>
			<label><?php echo getRadio('pohlavi', 'm');?>Muž</label>&nbsp;
			<label><?php echo getRadio('pohlavi', 'f');?>Žena</label>
		</td>
		<td><?php
			$error = checkPostField("/m|f/i", "pohlavi") || $error;
		?></td>
	</tr><tr>
		<td>E-mail: </td>
		<td><input type="text" name="email" value="<?php echo post("email"); ?>" /></td>
		<td>Max. 50 znaků, ve formátu e-mail adresy<?php
			$error = (strlen(post("email")) < 1 ? 0 : 
				checkPostField("/^\b[A-Z0-9._%-]+@[A-Z0-9.-]+\.[A-Z]{2,4}\b$/i", "email")) || $error;
		?></td>
	</tr><tr>
		<td>Telefon: </td>
		<td><input type="text" name="telefon" value="<?php echo post("telefon"); ?>" /></td>
		<td>Pouze číslice a +, max. 13 znaků<?php
			$error = (strlen(post("telefon")) < 1 ? 0 : 
				checkPostField("/^((\+|00)\d{3})? ?( ?\d{3}){3}$/", "telefon")) || $error;
		?></td>
	</tr><tr>
		<td>Datum narození: (*)</td>
		<td><?php echo Helper::get()->date()->name('narozeni')->textBox(); ?></td>
		<td><?php $error = checkDateString(Helper::get()->date(false)->getPost()) || $error; ?></td>
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
			echo '<div class="box" title="', $skupina['us_popis'], '" ',
				'style="background-color:', Settings::$barvy[$skupina['us_color']][1], ';"></div>';
			echo '<label>', getRadio('skupina', $skupina['us_id']),
				$skupina['us_popis'], '</label><br/>';
		}
		?></td>
		<td></td>
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
<a href="/admin/users">Zpět</a>