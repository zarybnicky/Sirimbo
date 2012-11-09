<?php
$error = false;

global $isConfirmation;
$isConfirmation = (post("castka") || post("placeno"));

if(Request::getAction() == 'edit')
	$header = "Upravit platbu";
elseif(Request::getAction() == 'add')
	$header = "Přidat platbu";
else
	$header = "Neznámá akce";

header_minor($header);

$skupiny = DBSkupiny::getSkupiny();
echo '<div style="float:right;width:350px;">';
echo '<table style="float:right;">';
echo '<tr><td>Skupina</td><td>Kč/měsíc</td><td>Kč/čtvrtrok</td><td>Kč/půlrok</td></tr>';
foreach($skupiny as $skupina) {
	echo '<tr><td><div class="box" title="', $skupina['us_popis'], '" ',
		'style="background-color:', Settings::$barvy[$skupina['us_color']][1] . ';"></div>&nbsp;',
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
		<td><input type="text" name="castka" value="<?php echo post('castka'); ?>" />
			<?php $error = checkPostField("/^[0-9]+$/i", "castka") || $error; ?>
		</td>
	</tr><tr>
		<td>Placeno: (*)</td>
		<td><?php echo Helper::get()->date()->name('placeno')->textBox();?>
			<?php $error = checkDateString(Helper::get()->date(false)->getPost()) || $error; ?>
		</td>
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
		<?php if(Request::getAction() == 'add') {?>
		<td><button type="submit" name="action" value="add_confirm">Přidat</button></td>
		<?php }	else {?>
		<td><button type="submit" name="action" value="edit_confirm">Upravit</button></td>
		<?php } ?>
		<td>Pole označená (*) jsou povinná</td>
	</tr>
</table>
</form>
<a href="<?php echo Request::getReferer(); ?>">Zpět</a>