<?php
$error = false;

global $isConfirmation;
$isConfirmation = (post("color") || post("platba"));

$request = explode('/', $_SERVER['REQUEST_URI']);
$action = array_slice($request, 3);
if(empty($action)) $action[] = '';

if($action[0] == 'edit')
	$header = "Upravit skupinu";
elseif($action[0] == 'add')
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
		$s = new SelectHelper();
		$s->select()
			->name('color')
			->get(false);
		foreach(Settings::$barvy as $name => $barva)
			$s->option($name, $barva[0]);
		echo $s;
		?>
		</td>
	</tr><tr>
		<td>Měsíční platba (*): </td>
		<td><input type="text" name="platba" value="<?php echo post("platba"); ?>" /></td>
		<td>Pouze číslice<?php
			$error = checkPostField("/^[0-9]+$/i", "platba") || $error;
		?></td>
	</tr><tr>
		<td>Popis: (*)</td>
		<td><textarea rows="15" cols="50" name="popis"><?php echo post('popis');?></textarea></td>
		<td></td>
	</tr><tr>
		<?php if($action[0] == 'add') {?>
		<td><button type="submit" name="action" value="add_confirm">Přidat</button></td>
		<?php }	else {?>
		<td><button type="submit" name="action" value="edit_confirm">Upravit</button></td>
		<?php } ?>
		<td>Pole označená (*) jsou povinná</td>
		<td></td>
	</tr>
</table>
</form>
<a href="/admin/skupiny">Zpět</a>