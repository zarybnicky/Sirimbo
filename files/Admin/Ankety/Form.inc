<?php
$request = explode('/', $_SERVER['REQUEST_URI']);
$action = array_slice($request, 3);
if(empty($action)) $action[] = '';

if($action[0] == 'edit')
	$header = "Upravit anketu";
elseif($action[0] == 'add')
	$header = "Přidat anketu";
else
	$header = "Neznámá akce";

header_minor($header);
?>
<form action="<?php echo $_SERVER['REQUEST_URI']; ?>" method="post">
<button type="submit" name="action" value="enter" style="display:none;">Enter</button>
<table>
	<tr>
		<td>Jméno: </td>
		<td><input type="text" name="jmeno" value="<?php echo getPostField('jmeno');?>" /></td>
	</tr>
	<tr>
		<td>Text: </td>
		<td><input type="text" name="text" value="<?php echo getPostField('text');?>" /></td>
	</tr><tr>
		<td>Viditelný: </td>
		<td><input type="checkbox" name="visible" value="visible" <?php
			if(getPostField('visible')) echo 'checked="checked"';
			if(!User::checkPermissionsBool(L_ADMIN)) echo 'readonly="readonly"'
		?>/></td>
	</tr><tr>
		<td>Text</td><td>Počet hlasů</td>
	</tr>
	<?php
	if(!empty($items)) {
		foreach($items as $item) {
				echo '<tr>';
				echo '<td><input type="text" name="', $item['aki_id'], '-text" value="', $item['aki_text'],
					'"/></td>';
				echo '<td>', isset($data['aki_pocet']) ? (int) $data['aki_pocet'] : 0, '</td>';
				echo '<td><button type="submit" name="remove" value="', $item['aki_id'], '">',
					'Odstranit a uložit</button></td>';
				echo '</tr>';
		}
	}
	?>
	<tr>
		<td><input type="text" name="add_text" /></td>
		<td>0</td>
		<td><button type="submit" name="action" value="confirm">Přidat a uložit</button></td>
	</tr><tr>
		<td><button type="submit" name="action" value="confirm">Uložit</button></td>
		<td><a href="/admin/ankety">Zpět</a></td>
	</tr>
</table>
</form>