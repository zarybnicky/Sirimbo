<?php
$error = false;

global $isConfirmation;
$isConfirmation = post("nadpis") || post("od") ||post('kat');

$request = explode('/', $_SERVER['REQUEST_URI']);
$action = array_slice($request, 3);
if(empty($action)) $action[] = '';

if($action[0] == 'edit')
	$header = "Upravit inzerát";
elseif($action[0] == 'add')
	$header = "Přidat inzerát";
else
	$header = "Neznámá akce";
header_minor($header);
?>
<form action="<?php echo $_SERVER['REQUEST_URI']; ?>" method="POST">
<table>
	<tr>
		<td>Nadpis (*): </td>
		<td><input type="text" name="nadpis" value="<?php echo post("nadpis"); ?>" /></td>
		<td>Max. 255 znaků<?php
			$error = checkPostFieldLength(1, 255, "nadpis") || $error;
		?></td>
	</tr><tr>
		<td>Text: </td>
		<td><textarea rows="15" cols="40" name="text"><?php echo post("text"); ?></textarea></td>
		<td></td>
	</tr><tr>
		<td>Kategorie (*): </td>
		<td>
			<input type="radio" name="kat" value="1" <?php
				if(post("kat") == "1") echo 'checked="checked"';
			?>/>Prodám&nbsp;&nbsp;<input type="radio" name="kat" value="2" <?php
				if(post("kat") == "2") echo 'checked="checked"';
			?>/>Koupím<br/>
			<input type="radio" name="kat" value="3" <?php
				if(post("kat") == "3") echo 'checked="checked"';
			?>/>Hledám partnera&nbsp;&nbsp;<input type="radio" name="kat" value="4" <?php
				if(post("kat") == "4") echo 'checked="checked"';
			?>/>Hledám partnerku<br/>
		</td>
		<td><?php
			$error = checkPostField("/1|2|3|4/i", "kat") || $error;
		?></td>
	</tr><tr>
		<td>Uživatel: </td>
		<td><?php if(post('reg') <= 0) {
				echo '<input type="hidden" name="jmeno" value="', post('jmeno'), '" />';
				echo '<input type="hidden" name="prijmeni" value="', post('prijmeni'), '" />';
				echo post('jmeno'), ' ', post('prijmeni'), '<br/>';
			}
			$userselect = new UserSelectHelper();
			echo $userselect->name('reg')->users(DBUser::getUsers());
		?></td><td></td>
	</tr><tr>
		<td>Od: (*)</td>
		<td><?php echoDateSelect("od_day", "od_month", "od_year");?></td>
		<td><?php
			$od_str = post("od_year") . "-" . post("od_month") . "-" .
				post("od_day");
			$error = checkDateString($od_str) || $error;
		?></td>
	</tr><tr>
		<td>Do: </td>
		<td><?php echoDateSelect("do_day", "do_month", "do_year");?></td>
		<td>(pokud je prázdný, počítá se jako "Od")<?php
			$do_str = post("do_year") . "-" . post("do_month") . "-" .
				post("do_day");
			$error = (($do_str == "0000-00-00") ? 0 : checkDateString($do_str)) || $error;
		?></td>
	</tr><tr>
		<td>Viditelný: </td>
		<td><input type="checkbox" name="visible" value="visible" <?php
			if(post("visible")) echo 'checked="checked"';
		?>/></td>
		<td></td>
	</tr><tr>
		<?php if($action[0] == 'add') { ?>
		<td><button type="submit" name="action" value="add_confirm">Přidat</button></td>
		<?php }	else {?>
		<td><button type="submit" name="action" value="edit_confirm">Upravit</button></td>
		<?php } ?>
		<td></td>
		<td>Pole označená (*) jsou povinná</td>
	</tr>
</table>
</form>
<a href="/admin/inzerce">Zpět</a>