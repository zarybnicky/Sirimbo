<?php
header_main('Správa akcí');
notice($this->redirect()->getMessage());
?>
<form action="" method="POST" class="centerWTable">
<button type="submit" name="action" value="enter" style="display:none;">Enter</button>
<?php echo $this->render('files/View/Member/Akce/Header.inc', $this->data);?>
<table>
<tr>
	<td>Uživatel</td>
	<td></td>
</tr>
<?php
$userselect = $this->userSelect()->users($this->users);
if(!empty($this->items)) {
	foreach($this->items as $item) {
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