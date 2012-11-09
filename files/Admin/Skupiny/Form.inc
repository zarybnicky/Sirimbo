<?php
$error = false;

global $isConfirmation;
$isConfirmation = (post("color") || post("platba"));

if(Request::getAction() == 'edit')
	$header = "Upravit skupinu";
elseif(Request::getAction() == 'add')
	$header = "Přidat skupinu";
else
	$header = "Neznámá akce";

header_minor($header);
?>
<form action="<?php echo $_SERVER['REQUEST_URI']; ?>" method="POST">
<table>
	<tr>
		<td>Barva (*): </td>
		<td><?php
		$s = Helper::get()->select()
			->post()->name('color');
		foreach(Settings::$barvy as $name => $barva)
			$s->option($name, $barva[0]);
		echo $s;
		?>
		</td>
	</tr><tr>
		<td>Částka za měsíc: </td>
		<td><input type="text" name="platba" value="<?php echo post("platba"); ?>" />
			<?php $error = checkPostField("/^$|^[0-9]+$/i", "platba") || $error; ?>
		</td>
	</tr><tr>
		<td>Částka za čtvrt roku: </td>
		<td><input type="text" name="platba_ctvrtrok" value="<?php echo post('platba_ctvrtrok'); ?>" />
			<?php $error = checkPostField("/^$|^[0-9]+$/i", "platba_ctvrtrok") || $error; ?>
		</td>
	</tr><tr>
		<td>Částka za půl roku: </td>
		<td><input type="text" name="platba_pulrok" value="<?php echo post('platba_pulrok'); ?>" />
			<?php $error = checkPostField("/^$|^[0-9]+$/i", "platba_pulrok") || $error; ?>
		</td>
	</tr><tr>
		<td></td>
		<td>Stačí zadat jednu z částek</td>
	</tr><tr>
		<td>Popis: (*)</td>
		<td><textarea rows="15" cols="50" name="popis"><?php echo post('popis');?></textarea></td>
	</tr><tr>
		<?php if(Request::getAction() == 'add') {?>
		<td><button type="submit" name="action" value="add_confirm">Přidat</button></td>
		<?php }	else {?>
		<td><button type="submit" name="action" value="edit_confirm">Upravit</button></td>
		<?php } ?>
		<td>Pole označená (*) jsou povinná</td>
	</tr>
</table>
</form>
<a href="/admin/skupiny">Zpět</a>