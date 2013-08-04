<?php
header_main('Správa plateb');
if(Request::getAction() == 'add')
	header_minor("Přidat platbu");
else
	header_minor("Upravit platbu");

notice(View::getRedirectMessage());
if(isset($f) && is_object($f))
	notice(implode('<br/>', $f->getMessages()));

$skupiny = DBSkupiny::getSkupiny();

$aktualni = Request::getAction() == 'edit' ? post('user') : get('u');
if($aktualni && is_numeric($aktualni) && ($user = DBUser::getUserData($aktualni))) { 
	echo '<div style="float:right;width:350px;">';
	echo '<div class="h_minor" style="float:right;">Aktuální uživatel</div>';
	echo '<table style="float:right;clear:right;">';
	echo '<tr><td>Jméno</td><td>', $user['u_prijmeni'], ', ', $user['u_jmeno'], '</td></tr>';
	echo '<tr><td>Skupina</td><td>', getColorBox($user['us_color'], $user['us_popis']), '</td></tr>';
	echo '</table>';
	echo '</div>';
}
echo '<div style="float:right;width:350px;clear:right;">';
echo '<table style="float:right;clear:right;">';
	echo '<div class="h_minor" style="float:right;">Přehled skupin</div>';
echo '<tr><td>Skupina</td><td>Kč/měsíc</td><td>Kč/čtvrtrok</td><td>Kč/půlrok</td></tr>';
foreach($skupiny as $skupina) {
	echo '<tr><td>', getColorBox($skupina['us_color'], $skupina['us_popis']), '&nbsp;',
		$skupina['us_popis'], '</td>',
		'<td>', $skupina['us_platba_mesic'], '</td>',
		'<td>', $skupina['us_platba_ctvrtrok'], '</td>',
		'<td>', $skupina['us_platba_pulrok'], '</td></tr>';
}
echo '</table>';
echo '</div>';
?>
<form action="<?php echo Request::getURI(); ?>" method="POST">
<table>
	<tr>
		<td>Uživatel (*): </td>
		<td><?php
		if(Request::getAction() == 'add')
			post('user', get('u'));
		$users = DBUser::getUsers();
		$s = Helper::get()->select()
			->post()->name('user');
		foreach($users as $user)
			$s->option($user['u_id'], $user['u_prijmeni'] . ', ' . $user['u_jmeno']);
		echo $s;
		?>
		</td>
	</tr><tr>
		<td>Částka (*): </td>
		<td><input type="text" name="castka" value="<?php echo post('castka'); ?>" /></td>
	</tr><tr>
		<td>Placeno: (*)</td>
		<td><?php echo Helper::get()->date()->name('placeno')->textBox();?>	</td>
	</tr><tr>
		<td>Platební období (*): </td>
		<td><?php
		$s = Helper::get()->select();
		$s->post()->name('obdobi');
		foreach(Settings::$platby_obdobi as $key => $value)
			$s->option($key, $value[2]);
		echo $s;
		?>
		<input type="hidden" name="referer" value="<?php echo Request::getReferer()?>" />
		</td>
	</tr><tr>
		<td><button type="submit"><?php
			if(Request::getAction() == 'add') echo 'Přidat';
			else echo 'Upravit';
		?></button></td>
		<td>Pole označená (*) jsou povinná</td>
	</tr>
</table>
</form>
<a href="<?php echo Request::getReferer(); ?>">Zpět</a>