<form action="<?php echo Request::getURI();?>" method="POST">
<?php
if(!TISK) {
	echo Helper::get()->menu()
		->float(MenuHelper::FLOAT_RIGHT)
		->content('Přidat', 'akce/add')
		->content('Upravit', 'edit', true)
		->content('Upravit přihlášené', 'edit_detail', true)
		->content('Upravit dokumenty', 'edit_doku', true)
		->content('Odstranit', 'remove', true);
}
?>
<div style="margin-right:150px;">
<table style="width:100%">
<tr>
<td></td>
<td>Jméno</td>
<td>Místo</td>
<td>Datum</td>
</tr>
<?php
$akce = DBAkce::getAkce();

foreach($akce as $item) {
	echo '<tr>';
	
	echo '<td><input type="checkbox" name="akce[]" value="', $item['a_id'], '" /></td>';
	echo '<td>', $item['a_jmeno'], '</td>';
	echo '<td>', $item['a_kde'], '</td>';
	echo '<td>', formatDate($item['a_od']);
	if($item['a_od'] != $item['a_do'])
		echo ' - ', formatDate($item['a_do']);
	echo '</td>';
	echo '</tr>';
}
?>
</table>
</div>
</form>