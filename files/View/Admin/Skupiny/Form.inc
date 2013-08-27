<?php
header_main('Správa skupin');
if($this->action == 'add')
	header_minor("Přidat skupinu");
else
	header_minor("Upravit skupinu");

notice($this->redirect()->getRedirectMessage());
?>
<form action="" method="POST">
<table>
	<tr>
		<td>Barva (*): </td>
		<td><?php
		$s = $this->select()
			->post()->name('color');
		foreach(Settings::$barvy as $name => $barva)
			$s->option($name, $barva[0]);
		echo $s;?>
		</td>
	</tr><tr>
		<td>Částka za měsíc: </td>
		<td><input type="text" name="platba" value="<?php echo post("platba"); ?>" /></td>
	</tr><tr>
		<td>Částka za čtvrtletí: </td>
		<td><input type="text" name="platba_ctvrtrok" value="<?php echo post('platba_ctvrtrok'); ?>" /></td>
	</tr><tr>
		<td>Částka za pololetí: </td>
		<td><input type="text" name="platba_pulrok" value="<?php echo post('platba_pulrok'); ?>" />	</td>
	</tr><tr>
		<td></td>
		<td>Stačí zadat jednu z částek</td>
	</tr><tr>
		<td>Popis: (*)</td>
		<td><textarea rows="15" cols="50" name="popis"><?php echo post('popis');?></textarea></td>
	</tr><tr>
		<td><button type="submit"><?php
			echo $this->action == 'add' ? 'Přidat' : 'Upravit';
		?></button></td>
		<td>Pole označená (*) jsou povinná</td>
	</tr>
</table>
</form>
<a href="/admin/skupiny">Zpět</a>