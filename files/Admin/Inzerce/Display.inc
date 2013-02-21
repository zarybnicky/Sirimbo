<?php
header_main('Správa inzerce');
notice(View::getRedirectMessage());
?>
<form action="<?php echo Request::getURI();?>" method="POST">
<?php
if(!TISK) {
	echo Helper::get()->menu()
		->float(MenuHelper::FLOAT_RIGHT)
		->content('Přidat', 'inzerce/add')
		->content('Upravit', 'edit', true)
		->content('Odstranit', 'remove', true)
		->content('Nepotvrzené inzeráty', 'inzerce/unconfirmed');
}
?>
<div style="margin-right:150px;">
<table style="width:100%">
<tr>
<td></td>
<td>Nadpis</td>
<td>Jméno</td>
<td>Kategorie</td>
<td>Viditelný</td>
</tr>
<?php
$inzerce = DBInzerce::getInzerce(false, true);

foreach($inzerce as $item) {
	echo '<tr>';
	echo '<td><input type="checkbox" name="inzerce[]" value="' . $item['i_id'] .
		'" /></td>';
	echo '<td>', $item['i_jmeno'], ' ', $item['i_prijmeni'], '</td>';
	echo '<td>' . formatDate($item['i_od']) . ' - ' . formatDate($item['i_do']) .  '</td>';
	echo '<td>';
	switch($item['i_kat']) {
		case INZERCE_KOUPIM: echo 'Koupím'; break;
		case INZERCE_PRODAM: echo 'Prodám'; break;
		case INZERCE_PARTNER: echo 'Hledám partnera'; break;
		case INZERCE_PARTNERKA: echo 'Hledám partnerku'; break;
	}
	echo '</td>';
	echo '<td>' . ($item['i_visible'] ? '&#10003;' : '') . '</td>';
	echo '</tr>';
}
?>
</table>
</div>
</form>