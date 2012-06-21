<form action="<?php echo $_SERVER["REQUEST_URI"] ?>" method="POST" class="centerWTable">
<button type="submit" name="action" value="enter" style="display:none;">Enter</button>
<?php DisplayAkce::viewAkceHeader($akce);?>
<table>
<tr>
<td>Uživatel</td><td>Rok narození</td><td></td>
</tr>
<?php
if(!empty($items)) {
	$userselect = new UserSelectHelper();
	
	foreach($items as $item) {
		echo '<tr>';
		echo '<td>';
		if($item['ai_user'] > 0)
			$_POST[$item['ai_id'] . '-user'] = $item['ai_user'];
		echo $userselect->users($users)->name($item['ai_id'] . '-user');
		echo '</td>';
		echo '<td><input type="text" size="4" name="', $item['ai_id'], '-rok" ',
			'value="', $item['ai_rok_narozeni'], '" /></td>';
		echo '<td><button type="submit" name="remove" value="', $item['ai_id'],
			'">Odstranit</button></td>';
		echo '</tr>';
	}
}
?>
<tr>
<td><?php echo $userselect->users($users)->name('add-user'); ?></td>
<td><input type="text" size="4" name="add-rok" /></td>
<td><button type="submit" name="add" value="add">Přidat</button></td>
</tr>
<tr><td><button type="submit" name="action" value="submit">Uložit</button></td></tr>
</table>
<a href="/admin/akce">Zpět</a>
</form>