<?php
$error = false;

global $isConfirmation;
$isConfirmation = getPostField("jmeno") || getPostField("kat") ||getPostField('text');

$request = explode('/', $_SERVER['REQUEST_URI']);
$action = array_slice($request, 3);
if(empty($action)) $action[] = '';

if($action[0] == 'edit')
	$header = "Upravit článek";
elseif($action[0] == 'add')
	$header = "Přidat článek";
else
	$header = "Neznámá akce";
header_minor($header);
?>
<form action="<?php echo $_SERVER['REQUEST_URI']; ?>" method="POST">
<table>
	<tr>
		<td>Jméno (*): </td>
		<td><input type="text" name="jmeno" value="<?php echo getPostField("jmeno"); ?>" /></td>
		<td>Max. 255 znaků<?php
			$error = checkPostFieldLength(1, 255, "jmeno") || $error;
		?></td>
	</tr><tr>
		<td>Text: </td>
		<td><textarea rows="15" cols="40" name="text"><?php echo getPostField("text"); ?></textarea></td>
		<td></td>
	</tr><tr>
		<td>Kategorie (*): </td>
		<td>
			<input type="radio" name="kat" value="1" <?php
				if(getPostField("kat") == "1") echo 'checked="checked"';
			?>/>Články&nbsp;&nbsp;<input type="radio" name="kat" value="2" <?php
				if(getPostField("kat") == "2") echo 'checked="checked"';
			?>/>Videa&nbsp;&nbsp;<input type="radio" name="kat" value="3" <?php
				if(getPostField("kat") == "3") echo 'checked="checked"';
			?>/>Krátké zprávy
		</td>
		<td><?php
			$error = checkPostField("/1|2\3/i", "kat") || $error;
		?></td>
	</tr><tr>
		<td>Fotogalerie: </td>
		<td><?php
			$dirs = DBGalerie::getDirs(true, true);

			if(!empty($dirs)) {
				echo '<select name="foto">';
				echo '<option value="none" selected="selected">Vyber si :o)</option>';
				foreach($dirs as $dir) {
					echo '<option value="', $dir['gd_id'], '"',
						(getPostField('foto') == $dir['gd_id'] ? ' selected="selected"' : ''), '>',
						str_repeat("-", $dir['gd_level'] - 1), $dir['gd_name'], '</option>';
				}
				echo '</select>';
			}
		?></td>
	</tr><tr>
		<?php if($action[0] == 'add') { ?>
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