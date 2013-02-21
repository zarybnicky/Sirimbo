<?php
header_main('Správa uživatelů');
header_minor('Nepotvrzení uživatelé');
notice(View::getRedirectMessage());
?>
<form action="<?php echo $_SERVER["REQUEST_URI"]?>" method="POST">
<?php
echo Helper::get()->menu()
	->float(MenuHelper::FLOAT_RIGHT)
	->content('Potvrdit', 'confirm', true)
	->content('Odstranit', 'remove', true)
	->content('Zpět', '../users');
?>
<table>
<tr>
<td></td>
<td>Úroveň</td>
<td>Skupina</td>
<td>Aktivní<br/>tanečník</td>
<td>Jméno</td>
<td>Email</td>
<td>Datum<br/>narozeni</td>
</tr>
<?php
$users = DBUser::getNewUsers();

$groups = DBPermissions::getGroups();
$s_group = new SelectHelper();
foreach($groups as $group)
	$s_group->option($group['pe_id'], $group['pe_name']);

$skupiny = DBSkupiny::getSkupiny();
$s_skupina = new SelectHelper();
foreach($skupiny as $skupina)
	$s_skupina->option($skupina['us_id'], $skupina['us_popis']);

foreach($users as $user) {
	echo '<tr>';
	echo '<td><input type="checkbox" name="users[]" value="' . $user['u_id'] . '" /></td>';
	echo '<td>';
	echo $s_group->post()->name($user['u_id'] . '-group');
	echo '</td>';
	echo '<td>';
	echo $s_skupina->post()->name($user['u_id'] . '-skupina');
	echo '</td>';
	echo '<td><input type="checkbox" name="' . $user['u_id'] . '-dancer" value="dancer" /></td>';
	echo '<td style="max-width:140px;">';
	echoFullJmeno($user);
	echo '</td>';
	echo '<td>', $user['u_email'], '</td>';
	echo '<td>', formatDate($user['u_narozeni']), '</td>';
	echo '</tr>';
}
?>
</table>
</form>