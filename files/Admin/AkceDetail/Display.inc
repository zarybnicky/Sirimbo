<form action="<?php echo $_SERVER["REQUEST_URI"] ?>" method="POST" class="centerWTable">
<button type="submit" name="action" value="enter" style="display:none;">Enter</button>
<?php DisplayAkce::viewAkceHeader($akce);?>
<table>
<tr>
<td>Registrovaný<br/>uživatel</td><td>Uživatel</td>
<td>Jméno</td><td>Příjmení</td><td>Rok narození</td><td></td>
</tr>
<?php
if(!empty($items)) {
	foreach($items as $item) {
		echo '<tr>';
		echo '<td><input type="checkbox" name="', $item['ai_id'], '-reg"';
		if($item['ai_user'] > 0)
			echo 'checked="checked"';
		echo ' /></td>';
		echo '<td>';
		if($item['ai_user'] > 0)
			$_POST[$item['ai_id'] . '-user'] = $item['ai_user'];
		echoUsers($item['ai_id'] . '-user', $users);
		echo '</td>';
		echo '<td><input type="text" size="10" name="', $item['ai_id'], '-jmeno" ',
			'value="', $item['ai_jmeno'], '" /></td>';
		echo '<td><input type="text" size="10" name="', $item['ai_id'], '-prijmeni" ',
			'value="', $item['ai_prijmeni'], '" /></td>';
		echo '<td><input type="text" size="4" name="', $item['ai_id'], '-rok" ',
			'value="', $item['ai_rok_narozeni'], '" /></td>';
		echo '<td><button type="submit" name="remove" value="', $item['ai_id'],
			'">Odstranit</button></td>';
		echo '</tr>';
	}
}
?>
<tr>
<td><input type="checkbox" name="add-reg" /></td>
<td><?php echoUsers("add-user", $users);?></td>
<td><input type="text" size="10" name="add-jmeno" /></td>
<td><input type="text" size="10" name="add-prijmeni" /></td>
<td><input type="text" size="4" name="add-rok" /></td>
<td><button type="submit" name="add" value="add">Přidat</button></td>
</tr>
<tr><td><button type="submit" name="action" value="submit">Uložit</button></td></tr>
</table>
<a href="/admin/akce">Zpět</a>
</form>