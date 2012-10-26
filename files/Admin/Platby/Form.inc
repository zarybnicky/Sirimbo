<?php
$error = false;

global $isConfirmation;
$isConfirmation = (post("castka") || post("placeno_year"));

$request = explode('?', $_SERVER['REQUEST_URI']);
$request = explode('/', $request[0]);
$action = array_slice($request, 3);
if(empty($action)) $action[] = '';

if($action[0] == 'edit')
	$header = "Upravit platbu";
elseif($action[0] == 'add')
	$header = "Přidat platbu";
else
	$header = "Neznámá akce";

header_minor($header);
?>
<form action="<?php echo $_SERVER['REQUEST_URI']; ?>" method="POST">
<table>
	<tr>
		<td>Uživatel (*): </td>
		<td><?php
		if($action[0] == 'add')
			post('user', get('u'));
		$users = DBUser::getUsers();
		$s = new SelectHelper();
		$s->select()
			->name('user')
			->get(false);
		foreach($users as $user)
			$s->option($user['u_id'], $user['u_prijmeni'] . ', ' . $user['u_jmeno']);
		echo $s;
		?>
		</td>
	</tr><tr>
		<td>Částka (*): </td>
		<td><input type="text" name="castka" value="<?php echo post('castka'); ?>" /></td>
		<?php $error = checkPostField("/^[0-9]+$/i", "castka") || $error; ?>
	</tr><tr>
		<td>Placeno: (*)</td>
		<td><?php echoDateSelect("placeno_day", "placeno_month", "placeno_year");?></td>
		<?php
			$od_str = post("placeno_year") . "-" . post("placeno_month") . "-" .
				post("placeno_day");
			$error = checkDateString($od_str) || $error;
		?>
	</tr><?php if($action[0] == 'edit') {?><tr>
		<td>Přepočítat<br/>platnost: (*)</td>
		<td><?php echo getCheckbox('prepocitat', 'prepocitat', false); ?></td>
	</tr><?php } ?><tr>
		<?php if($action[0] == 'add') {?>
		<td><button type="submit" name="action" value="add_confirm">Přidat</button></td>
		<?php }	else {?>
		<td><button type="submit" name="action" value="edit_confirm">Upravit</button></td>
		<?php } ?>
		<td>Pole označená (*) jsou povinná</td>
	</tr>
</table>
</form>
<a href="/admin/platby">Zpět</a>