<?php
header_main('Správa uživatelů');
header_minor('Zabanovaní uživatelé');
notice(View::getRedirectMessage());
?>
<form action="<?php echo Request::getURI()?>" method="POST">
<?php
echo Helper::get()->menu()
	->float(MenuHelper::FLOAT_RIGHT)
	->content('Upravit', 'edit', true)
	->content('Odstranit', 'remove', true)
	->content('Zpět', '../users');
?>
<div>
<table style="width:100%">
<tr>
<td></td>
<td></td>
<td>Jméno</td>
<td>Email</td>
<td>Telefon</td>
<td>Datum<br/>narozeni</td>
<td>Poslední úprava</td>
</tr>
<?php
$users = DBUser::getBannedUsers();
foreach($users as $user) {
	echo '<tr>';
	echo '<td><input type="checkbox" name="users[]" value="' . $user['u_id'] . '" /></td>';
	echo '<td>', getColorBox($user['us_color'], $user['us_popis']), '</td>';
	echo '<td style="max-width:140px;">', $user['u_prijmeni'], ', ', $user['u_jmeno'];
	echo '<td>', $user['u_email'], '</td>';
	echo '<td>', $user['u_telefon'], '</td>';
	echo '<td>', formatDate($user['u_narozeni']), '</td>';
	echo '<td>', formatTimestamp($user['u_aktu']), '</td>';
	echo '</tr>';
}
?>
</table>
</div>
</form>