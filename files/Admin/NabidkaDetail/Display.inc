<form action="<?php echo $_SERVER["REQUEST_URI"] ?>" method="POST" class="centerWTable">
<button type="submit" name="action" value="enter" style="display:none;">Enter</button>
<div class="n_wrapper">
<?php DisplayNabidka::viewNabidkaHeader($data, $obsazeno); ?>
<table>
<tr><td>Tanečník</td><td>Počet hodin</td></tr>
<?php
$userselect = new UserSelectHelper();
if(!empty($items)) {
	foreach($items as $item) {
			echo '<tr>';
			post($item['ni_id'] . '-partner', $item['ni_partner']);
			echo '<td>';
			echo $userselect->userSelect()->name($item['ni_id'] . '-partner')
				->idVar('p_id')->type('par')->users($users);
			echo '</td>';
			echo '<td><input type="text" name="', $item['ni_id'], '-hodiny" value="', $item['ni_pocet_hod'],
				'" size=1/></td>';
			echo '<td><button type="submit" name="remove" value="', $item['ni_id'], '">',
				'Odstranit</button></td>';
			echo '</tr>';
	}
}
?>
<tr>
<td><?php echo $userselect->userSelect()->name('add_partner')
	->idVar('p_id')->type('par')->users($users);?></td>
<td><input type="text" name="add_hodiny" value=""  size=1/></td>
<td><button type="submit" name="action" value="add">Přidat</button></td>
</tr>
<tr><td><button type="submit" name="action" value="submit">Uložit</button></td></tr>
</table>
</div>
<a href="/admin/nabidka">Zpět</a>
</form>