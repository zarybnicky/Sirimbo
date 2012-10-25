<form action="/admin/skupiny" method="POST">
<?php
if(!TISK) {
	$f = new MenuHelper();
	echo $f->menu()
		->float(MenuHelper::FLOAT_RIGHT)
		->content('Přidat', 'skupiny/add')
		->content('Upravit', 'edit', true)
		->content('Odebrat', 'remove', true);
}
?>
<table>
<tr>
<td></td>
<td>Barvy</td>
<td>Měsíční platba</td>
<td>Popis</td>
</tr>
<?php
$skupiny = DBSkupiny::getSkupiny();
foreach($skupiny as $item) {
	echo "<tr>";
	echo '<td><input type="checkbox" name="skupiny[]" value="' . $item['us_id'] .	'" /></td>';
	echo '<td><div class="box" style="background-color:' .
		Settings::$barvy[$item['us_color']][1] . ';"></div></td>';
	echo '<td class="center">', $item['us_platba_mesic'], '</td>';
	echo '<td style="max-width:300px;">', $item['us_popis'], '</td>';
	echo '</tr>';
}
?>
</table>
</form>