<?php
header_main('Správa nabídka');
notice(View::getRedirectMessage());
?>
<form action="<?php echo Request::getURI();?>" method="POST">
<?php
if(!TISK) {
	echo Helper::get()->menu()
		->float(MenuHelper::FLOAT_RIGHT)
		->content('Přidat', 'nabidka/add')
		->content('Upravit', 'edit', true)
		->content('Upravit tréninky', 'edit_detail', true)
		->content('Odstranit', 'remove', true)
		->content('Uložit změny', 'save', true);
}
?>
<div style="margin-right:150px;">
<table style="width:100%">
<tr>
<td></td>
<td>Trenér</td>
<td>Datum</td>
<td>Viditelný</td>
</tr>
<?php
$nabidka = DBNabidka::getNabidka();

foreach($nabidka as $item) {
	echo '<tr>';
	
	if(Permissions::check('nabidka', P_OWNED, $item['n_trener']))
		echo '<td><input type="checkbox" name="nabidka[]" value="' . $item['n_id'] .
			'" /></td>';
	else
		echo '<td><input type="checkbox" name="nabidka[]" value="' .
			'" disabled="disabled" /></td>';
	echo '<td>';
	echoFullJmeno($item);
	echo '</td>';
	echo '<td>' . formatDate($item['n_od']) . ' - ' . formatDate($item['n_do']) .  '</td>';
	echo '<td>', getCheckbox($item['n_id'], '1', $item['n_visible']), '</td>';
	echo '</tr>';
}
?>
</table>
</div>
</form>