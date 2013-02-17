<form action="<?php echo $_SERVER["REQUEST_URI"] ?>" method="POST" class="centerWTable">
<button type="submit" name="action" value="enter" style="display:none;">Enter</button>
<?php DisplayAkce::viewAkceHeader($akce);?>
<table>
<tr>
	<td>Uživatel</td>
	<td></td>
</tr>
<?php
$userselect = Helper::get()->userSelect()->users($users);
if(!empty($items)) {
	foreach($items as $item) {
		echo '<tr>';
		echo '<td>';
		if($item['ai_user'] > 0)
			post($item['ai_id'] . '-user', $item['ai_user']);
		echo $userselect->name($item['ai_id'] . '-user');
		echo '</td>';
		echo '<td><button type="submit" name="remove" value="', $item['ai_id'],
			'">Odstranit</button></td>';
		echo '</tr>';
	}
}
?>
<tr>
	<td><?php echo $userselect->name('add-user'); ?></td>
	<td><button type="submit" name="add" value="add">Přidat</button></td>
</tr>
</table>
<button type="submit" name="action" value="submit">Uložit</button>
<a href="/admin/akce">Zpět</a>
</form>