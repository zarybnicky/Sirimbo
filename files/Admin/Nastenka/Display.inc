<?php
$pager = new Paging(new PagingAdapterDBSelect('DBNastenka'));
$pager->setCurrentPageField('p');
$pager->setItemsPerPageField('c');
$pager->setDefaultItemsPerPage(20);
$pager->setPageRange(5);
$items = $pager->getItems();
?>
<form action="<?php echo Request::getURI();?>" method="POST">
<?php
echo Helper::get()->menu()
	->float(MenuHelper::FLOAT_RIGHT)
	->content('Přidat', '/admin/nastenka/add')
	->content('Upravit', 'edit', true)
	->content('Odstranit', 'remove', true);

if(empty($items)) {
	notice('Žádné položky za toto období');
	return;
}
?>

<div style="margin-right:150px;">
<table style="width:100%">
<tr>
<td></td>
<td>Přidal</td>
<td>Datum</td>
<td>Skupiny</td>
<td>Nadpis</td>
</tr>
<?php
foreach($items as $item) {
	echo "<tr>";
	if(Permissions::canEditNastenka($item['up_kdo'], $item['up_lock']))
		echo '<td><input type="checkbox" name="nastenka[]" value="' . $item['up_id'] .
			'" /></td>';
	else
		echo '<td><input type="checkbox" name="nastenka[]" value="' .
			'" disabled="d" /></td>';
	echo '<td>';
	echoFullJmeno($item);
	echo '</td>';
	echo '<td>', formatTimestamp($item['up_aktu']), '</td>';
	echo '<td>';
	$skupiny = DBNastenka::getNastenkaSkupiny($item['up_id']);
	foreach($skupiny as $skupina)
		echo '<div class="box" title="', $skupina['ups_popis'], '" ',
			'style="background-color:', Settings::$barvy[$skupina['ups_color']][1], ';"></div>';
	echo '</td>';
	echo '<td>', $item['up_nadpis'], '</td>';
	echo '</tr>';
}
?>
</table>
<div style="text-align:center;">
<?php echo $pager->getNavigation(); ?>
</div>
</div>
</form>