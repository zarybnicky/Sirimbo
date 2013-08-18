<?php
header_main('Správa fotogalerie');
notice($this->redirect()->getRedirectMessage());
?>
<form action="<?php echo $_SERVER["REQUEST_URI"] ?>" method="POST">
<?php
echo Helper::get()->menu()
	->float(MenuHelper::FLOAT_RIGHT)
	->content('Přidat fotky', 'galerie/add')
	->content('Upravit fotky', 'edit', true)
	->content('Upravit složku', 'editdir', true)
	->content('Odebrat složku', 'remove', true)
	->content('Aktualizovat databázi', 'galerie/scan')
	->content('Uložit změny', 'save', true);

$dirs = DBGalerie::getDirs(true, true);

if(!empty($dirs)) {
	echo '<table><tr><td></td><td>Název</td><td>Skrytá</td></tr>';
	foreach($dirs as $item) {
		echo '<tr>';
		echo '<td><input type="checkbox" name="galerie[]" value="' . $item['gd_id'] . '" /></td>';
		echo '<td>', str_repeat("&nbsp;-&nbsp;", $item['gd_level'] - 1), $item['gd_name'], '</td>';
		echo '<td>', getCheckbox($item['gd_id'], '1', $item['gd_hidden']), '</td>';
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
<?php
$s = Helper::get()->select()
	->post()->name('parent')
	->option('none', '--');
foreach($dirs as $dir)
	$s->option($dir['gd_id'] . '-' . $dir['gd_level'],
		str_repeat("&nbsp;&nbsp;", $dir['gd_level'] - 1) . $dir['gd_name']);
?></td></tr>
<tr><td>Skrytá složka</td><td><input type="checkbox" name="hidden"/></td></tr>
<tr><td><button type="submit" name="action" value="adddir">Přidat složku</button></td></tr>
</table>
</form>