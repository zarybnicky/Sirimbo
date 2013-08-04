<?php
header_main('Správa inzerce');
if(Request::getAction() == 'add')
	header_minor("Přidat inzerát");
else
	header_minor('Upravit inzerát');

notice(View::getRedirectMessage());
if(isset($f) && is_object($f))
	notice(implode('<br/>', $f->getMessages()));
?>
<form action="<?php echo Request::getURI();?>" method="POST">
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
	</tr><tr>
		<td>Uživatel: </td>
		<td><?php if(post('reg') <= 0) {
				echo '<input type="hidden" name="jmeno" value="', post('jmeno'), '" />';
				echo '<input type="hidden" name="prijmeni" value="', post('prijmeni'), '" />';
				echo post('jmeno'), ' ', post('prijmeni'), '<br/>';
			}
			echo Helper::get()->userSelect()->name('reg')->users(DBUser::getUsers());
		?></td><td></td>
	</tr><tr>
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
		<td>Viditelný: </td>
		<td><label><?php echo getCheckbox('visible', 'visible')?></label></td>
		<td></td>
	</tr><tr>
	<td>
		<button type="submit" name="action">
		<?php echo (Request::getAction() == 'add' ? 'Přidat' : 'Upravit'); ?>
		</button>
	</td>
		<td></td>
		<td>Pole označená (*) jsou povinná</td>
	</tr>
</table>
</form>
<a href="/admin/inzerce">Zpět</a>