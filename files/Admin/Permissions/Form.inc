<?php
header_main('Správa oprávnění');
if(Request::getAction() == 'add')
	header_minor("Přidat uživatelskou skupinu");
else
	header_minor('Upravit uživatelskou skupinu');

notice(View::getRedirectMessage());
if(isset($f) && is_object($f))
	notice(implode('<br/>', $f->getMessages()));
?>
<form action="<?php echo Request::getURI();?>" method="POST">
<table>
	<tr>
		<td>Jméno: </td>
		<td colspan="<?php echo count(Settings::$permissions);?>">
			<input type="text" name="name" value="<?php echo post('name'); ?>" />
		</td>
	</tr><tr>
		<td>Popis: </td>
		<td colspan="<?php echo count(Settings::$permissions);?>">
			<textarea rows="7" cols="60" name="description"><?php echo post("description"); ?></textarea>
		</td>
	</tr><?php
	foreach(Settings::$permissions as $name => $item) {
		echo '<tr>';
		echo '<td>', $item['name'], '</td>';
		foreach(Settings::$permission_levels as $key => $level_name) {
			echo '<td>';
			if(isset($item[$key])) {
				echo '<label>', getRadio($name, $key, $item['default'] == $key), $level_name, '</label>';
			}
			echo '</td>';
		}
		echo '</tr>';
	}
	?>
</table>
<button type="submit" name="action"><?php echo (Request::getAction() == 'add' ? 'Přidat' : 'Upravit'); ?></button>
<a href="/admin/permissions">Zpět</a>
</form>