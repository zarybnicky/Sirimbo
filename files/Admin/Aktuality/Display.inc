<?php
header_main('Správa aktualit');
notice($this->redirect()->getRedirectMessage());
?>
<form action="" method="POST">
<?php
if(!TISK) {
	echo $this->menu()
		->float(MenuHelper::FLOAT_RIGHT)
		->content('Přidat', 'aktuality/add')
		->content('Upravit', 'edit', true)
		->content('Upravit galerii', 'foto', true)
		->content('Odebrat', 'remove', true);
}
?>
<div style="margin-right:150px">
<table style="width:100%">
<tr>
<td></td>
<td>Jméno</td>
<td>Kategorie</td>
</tr>
<?php
$aktuality = DBAktuality::getAktuality();
foreach($aktuality as $item) {
	echo '<tr>';
	
	if(Permissions::check('aktuality', P_OWNED, $item['at_kdo']))
		echo '<td><input type="checkbox" name="aktuality[]" value="' . $item['at_id'] .
			'" /></td>';
	else
		echo '<td><input type="checkbox" name="aktuality[]" value="' .
			'" disabled="disabled" /></td>';
	echo '<td>', $item['at_jmeno'], '</td>';
	echo '<td>';
	if($item['at_kat'] == AKTUALITY_CLANKY) echo "Články";
	elseif($item['at_kat'] == AKTUALITY_VIDEA) echo "Videa";
	elseif($item['at_kat'] == AKTUALITY_KRATKE) echo "Krátké zprávy";
	echo '</td>';
	echo '</tr>';
}
?>
</table>
</div>
</form>