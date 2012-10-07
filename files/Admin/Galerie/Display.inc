<form action="<?php echo $_SERVER["REQUEST_URI"] ?>" method="POST">
<?php
$f = new MenuHelper();
echo $f->menu()
	->float(MenuHelper::FLOAT_RIGHT)
	->content('Přidat fotky', 'galerie/add')
	->content('Upravit fotky', 'edit', true)
	->content('Upravit složku', 'editdir', true)
	->content('Odebrat složku', 'remove', true)
	->content('Aktualizovat databázi', 'galerie/scan');

$dirs = DBGalerie::getDirs(true, true);

if(!empty($dirs)) {
	echo '<table><tr><td></td><td>Název</td><td>Skrytá</td></tr>';
	foreach($dirs as $item) {
		echo '<tr>';
		echo '<td><input type="checkbox" name="galerie[]" value="' . $item['gd_id'] . '" /></td>';
		echo '<td>', str_repeat("&nbsp;-&nbsp;", $item['gd_level'] - 1), $item['gd_name'], '</td>';
		echo '<td>', ($item['gd_hidden'] ? '&#10003;' : ''), '</td>';
		echo '</tr>';
	}
	echo '</table>';
} else {
	notice('Žádné složky');
}
?>
<hr/>
<?php header_minor('Přidat složku');?>
<table>
<tr><td>Název</td><td><input type="text" name="name"/></td></tr>
<tr><td>Nadsložka</td><td>
	<select name="parent">
		<option value="none" selected="selected">Vyber si :o)</option><?php
foreach($dirs as $dir) {
	echo '<option value="', $dir['gd_id'], '-', $dir['gd_level'], '">',
		str_repeat("&nbsp;-&nbsp;", $dir['gd_level'] - 1), $dir['gd_name'], '</option>';
}?></select></td></tr>
<tr><td>Skrytá složka</td><td><input type="checkbox" name="hidden"/></td></tr>
<tr><td><button type="submit" name="action" value="adddir">Přidat složku</button></td></tr>
</table>
</form>