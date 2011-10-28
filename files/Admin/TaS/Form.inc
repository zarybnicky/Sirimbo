<?php
$error = false;

global $isConfirmation;
$isConfirmation = (getPostField("jmeno") || getPostField("misto"));

$request = explode('/', $_SERVER['REQUEST_URI']);
$action = array_slice($request, 3);
if(empty($action)) $action[] = '';

if($action[0] == 'edit')
	$header = "Upravit tábor/soustředění";
elseif($action[0] == 'add')
	$header = "Přidat tábor/soustředění";
else
	$header = "Neznámá akce";
header_minor($header);
?>
<form action="<?php
if($action[0] == 'edit') echo '/admin/tas';
else echo '/admin/tas/add'; ?>" method="POST">
<table>
	<tr>
		<td>Jméno akce (*): </td>
		<td><input type="text" name="jmeno" value="<?php echo getPostField("jmeno"); ?>" /></td>
		<td>Max. 255 znaků<?php
			$error = checkPostFieldLength(1, 255, "jmeno");
		?><input type="hidden" name="id" value="<?php echo getPostField("id");?>" /></td>
	</tr><tr>
		<td>Místo akce (*): </td>
		<td><input type="text" name="kde" value="<?php echo getPostField("kde"); ?>" /></td>
		<td>Max. 255 znaků<?php
			$error = checkPostFieldLength(1, 255, "kde") || $error;
		?></td>
	</tr><tr>
		<td>Další info: </td>
		<td><textarea rows="15" cols="50" name="info"><?php echo getPostField("info"); ?></textarea></td>
		<td></td>
	</tr><tr>
		<td>Od: (*)</td>
		<td><?php echoDateSelect("od_day", "od_month", "od_year");?></td>
		<td><?php
			$od_str = getPostField("od_year") . "-" . getPostField("od_month") . "-" .
				getPostField("od_day");
			$error = checkDateString($od_str) || $error;
		?></td>
	</tr><tr>
		<td>Do: </td>
		<td><?php echoDateSelect("do_day", "do_month", "do_year");?></td>
		<td>(pokud je prázdný, počítá se jako "Od")<?php
			$do_str = getPostField("do_year") . "-" . getPostField("do_month") . "-" .
				getPostField("do_day");
			$error = (($do_str == "0000-00-00") ? 0 : checkDateString($do_str)) || $error;
		?></td>
	</tr><tr>
		<td>Kapacita (*): </td>
		<td><input type="text" name="kapacita" value="<?php echo getPostField("kapacita"); ?>" /></td>
		<td>Pouze číslice<?php
			$error = checkPostField("/^[0-9]{0,11}/", "kapacita") || $error;
		?></td>
	</tr><tr>
		<td>Dokumenty: </td>
		<td><?php
			if(!getPostField("dokumenty")) echo "Žádné dokumenty";
			else {
				foreach(getPostField("dokumenty") as $id) {
					$data = DBDokumenty::getSingleDokument($id);
					echo "<a href=\"/member/download?id=", $id, "\">", $data["d_name"], "</a>";
				}
			}
		?></td>
	</tr><tr>
		<td>Uzamknout: </td>
		<td><input type="checkbox" name="lock" value="lock" <?php
			if(getPostField("lock")) echo "checked=\"checked\"";
		?>/></td>
		<td></td>
	</tr><tr>
		<?php if($action[0] == 'add') {?>
		<td><button type="submit" name="action" value="add_confirm">Přidat</button></td>
		<?php }	else {?>
		<td><button type="submit" name="action" value="edit_confirm">Upravit</button></td>
		<?php } ?>
		<td></td>
		<td>Pole označená (*) jsou povinná</td>
	</tr>
</table>
</form>

<a href="/admin/tas">Zpět</a>