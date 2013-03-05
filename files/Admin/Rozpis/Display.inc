<?php
header_main('Správa rozpisů');
notice(View::getRedirectMessage());
?>
<form action="<?php echo Request::getURI();?>" method="POST">
<?php
if(!TISK) {
	echo Helper::get()->menu()
		->float(MenuHelper::FLOAT_RIGHT)
		->content('Přidat', 'rozpis/add')
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
<td>Místo</td>
<td>Viditelný</td>
</tr>
<?php
$rozpis = DBRozpis::getRozpis();

foreach($rozpis as $item) {
	echo '<tr>';
	if(Permissions::check('rozpis', P_OWNED, $item['r_trener']))
		echo '<td><input type="checkbox" name="rozpis[]" value="', $item['r_id'] .
			'" /></td>';
	else
		echo '<td><input type="checkbox" name="rozpis[]" value="',
			'" disabled="d" /></td>';
	echo '<td>';
	echoFullJmeno($item);
	echo '</td>';
	echo '<td>', formatDate($item['r_datum']),  '</td>';
	echo '<td>', $item['r_kde'], '</td>';
	echo '<td>', getCheckbox($item['r_id'], '1', $item['r_visible']), '</td>';
	echo '</tr>';
}
?>
</table>
</div>
</form>