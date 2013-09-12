<?php
header_main('Správa aktualit');
if(Request::getAction() == 'add')
	header_minor("Přidat článek");
else
	header_minor("Upravit článek");

notice($this->redirect()->getMessage());
?>
<form action="" method="POST">
<table>
	<tr>
		<td>Jméno (*): </td>
		<td><input type="text" name="jmeno" value="<?php echo post("jmeno"); ?>" /></td>
		<td>Max. 255 znaků</td>
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
		<td></td>
	</tr><tr>
		<td>Fotogalerie: </td>
		<td><?php
			$dirs = DBGalerie::getDirs(true, true);
			
			$s = $this->select()
				->post()->name('foto')
				->value(post('foto'))
				->option('none', '--');
			foreach($dirs as $dir)
				$s->option($dir['gd_id'],
					str_repeat("-", $dir['gd_level'] - 1) . $dir['gd_name']);
			echo $s;
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
<a href="/admin/aktuality">Zpět</a>