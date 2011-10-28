<form action="<?php echo $_SERVER["REQUEST_URI"] ?>" method="POST">
<button type="submit" name="action" value="enter" style="display:none;">Enter</button>
<div class="r_wrapper">
<?php DisplayRozpis::viewRozpisHeader($roz); ?>
<table>
<tr><td>Tanečník</td><td>Od</td><td>Do</td><td></td><td>Zámek</td></tr>
<?php
if(!empty($items)) {
	foreach($items as $item) {
		echo '<tr>';
		echo '<td>';
		$_POST[$item['ri_id'] . '-partner'] = $item['ri_partner'];
		echoPartners($item['ri_id'] . '-partner', $users);
		echo '</td>';
		echo '<td><input type="text" name="', $item['ri_id'], '-od" value="',
			formatTime($item['ri_od'], 1), '"/></td>';
		echo '<td><input type="text" name="', $item['ri_id'], '-do" value="',
			formatTime($item['ri_do'], 1), '"/></td>';
		echo '<td><button type="submit" name="remove" value="', $item['ri_id'],
			'">Odstranit</button></td>';
		echo '<td><input type="checkbox" name="', $item['ri_id'],
			'-lock" value="lock"';
		if($item['ri_lock']) echo 'checked="checked"';
		echo ' /></td>';
		echo '</tr>';
	}
}
?>
<tr>
<td><?php echoPartners('add_partner', $users);?></td>
<td><input type="text" name="add_od" value="" /></td>
<td><input type="text" name="add_do" value="" /></td>
<td><button type="submit" name="action" value="add">Přidat</button></td>
<td><input type="checkbox" name="add_lock" value="lock" /></td>
</tr>
<tr><td><button type="submit" name="action" value="submit">Uložit</button></td></tr>
</table>
</div>
<br/>
<?php
header_minor('Nabídky:');
echoNabidky('n_id', DBNabidka::getNabidka());
?>
<button type="submit" name="action" value="select">Vybrat nabídku</button>
<?php
if(is_numeric(getPostField('n_id'))) {
	if(getPostField('n_id') < 0 && getPostField('n_id') > 2147483647)
		View::viewError(ER_CORRUPT_DATA);
	if(!($nab = DBNabidka::getSingleNabidka(getPostField('n_id'))))
		View::viewError(ER_CORRUPT_DATA);
	$items = DBNabidka::getNabidkaItem(getPostField('n_id'));
	$obsazeno = (int)DBNabidka::getNabidkaItemLessons(getPostField('n_id'));
	echo '<div class="n_wrapper">';
	DisplayNabidka::viewNabidkaHeader($nab, $obsazeno);
	echo '<table>';
	echo '<tr><td>Tanečník</td><td>Počet hodin</td></tr>';
	if(!empty($items)) {
		foreach($items as $item) {
			echo '<tr>';
			echo '<td>', echoFullJmeno($item), '</td>';
			echo '<td>', $item['ni_pocet_hod'], '</td>';
			echo '</tr>';
		}
	}
	echo '</table></div>';
}
?>
<br/><br/>
<?php header_minor("Pomocník: ");?>
<button type="submit" name="generate" value="overlap">Opravit přesahy</button><br/>
<button type="submit" name="generate" value="gen_add">Přidat</button>
<input type="text" name="gen_add_hod" size="2" /> hodin po
<input type="text" name="gen_add_len" size="3" /> minutách od
<input type="text" name="gen_add_od" size="5" /> hodin
</form>
<br/>
<a href="/admin/rozpis">Zpět</a>