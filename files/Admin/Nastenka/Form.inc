<?php
$error = false;

global $isConfirmation;
$isConfirmation = (post("nadpis") || post("text"));

$request = explode('/', $_SERVER['REQUEST_URI']);
$action = array_slice($request, 3);

if($action[0] == 'add')
	$header = "Přidat příspěvek";
elseif($action[0] == 'edit')
	$header = "Upravit příspěvek";
else
	$header = "Neznámá akce";

header_minor($header);
?>
<form action="/admin/nastenka" method="POST">
<table>
	<tr>
		<td>Nadpis: </td>
		<td><input type="text" name="nadpis" value="<?php echo post("nadpis"); ?>" />
			<input type="hidden" name="id" value="<?php $id = post("id");
			if(is_numeric($id)) echo $id; else if($id) View::viewError(ER_CORRUPT_DATA); ?>"/></td>
		<td></td>
	</tr><tr>
		<td>Text: </td>
		<td><textarea cols="50" rows="15" name="text"><?php echo post("text"); ?></textarea></td>
		<td></td>
	</tr><tr>
		<td>Uzamknout: </td>
		<td><?php echo getCheckbox('lock', 'lock');?></td>
		<td></td>
	</tr><tr>
		<td>Skupiny: </td>
		<td>
		<?php
		$skupiny = DBSkupiny::getSkupiny();
		foreach($skupiny as $skupina) {
			echo getCheckbox('sk-' . $skupina['us_id'], '1'), ' ';
			echo $skupina['us_popis'], ' ';
			echo '<div class="box" style="background-color:' .
				Settings::$barvy[$skupina['us_color']][1] . '"></div>';
			echo '<br/>';
		}
		?>
		</td>
		<td></td>
	</tr><tr>
		<?php if($action[0] == 'add') {?>
		<td><button type="submit" name="action" value="add_confirm">Přidat</button></td>
		<?php } else {?>
		<td><button type="submit" name="action" value="edit_confirm">Upravit</button></td>
		<?php }	?>
		<td></td>
		<td>Pole označená (*) jsou povinná</td>
	</tr>
</table>
</form>

<a href="/admin/nastenka">Zpět</a>