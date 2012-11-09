<?php
$error = false;

global $isConfirmation;
$isConfirmation = post("jmeno") || post("kat") || post('text');

if(Request::getAction() == 'edit')
	$header = "Upravit článek";
elseif(Request::getAction() == 'add')
	$header = "Přidat článek";
else
	$header = "Neznámá akce";
header_minor($header);
?>
<form action="<?php echo $_SERVER['REQUEST_URI']; ?>" method="POST">
<table>
	<tr>
		<td>Jméno (*): </td>
		<td><input type="text" name="jmeno" value="<?php echo post("jmeno"); ?>" /></td>
		<td>Max. 255 znaků<?php
			$error = checkPostFieldLength(1, 255, "jmeno") || $error;
		?></td>
	</tr><tr>
		<td>Text: </td>
		<td><textarea rows="15" cols="40" name="text"><?php echo post("text"); ?></textarea></td>
		<td></td>
	</tr><tr>
		<td>Kategorie (*): </td>
		<td>
			<label><?php echo getRadio('kat', 1);?>Články</label>&nbsp;
			<label><?php echo getRadio('kat', 2);?>Videa</label>&nbsp;
			<label><?php echo getRadio('kat', 3);?>Krátké zprávy</label>
		</td>
		<td><?php
			$error = checkPostField("/1|2\3/i", "kat") || $error;
		?></td>
	</tr><tr>
		<td>Fotogalerie: </td>
		<td><?php
			$dirs = DBGalerie::getDirs(true, true);
			
			$s = Helper::get()->select()
				->post()->name('foto')
				->value($dir['gd_id'])
				->option('none', '--');
			foreach($dirs as $dir)
				$s->option($dir['gd_id'],
					str_repeat("-", $dir['gd_level'] - 1) . $dir['gd_name']);
			echo $s;
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
<a href="/admin/aktuality">Zpět</a>