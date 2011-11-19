<?php
$error = false;

global $isConfirmation;
$isConfirmation = getPostField("nadpis") || getPostField("od") ||getPostField('kat');

$request = explode('/', $_SERVER['REQUEST_URI']);
$id = is_numeric($request[count($request) - 1]) ? array_pop($request) : '0';
$action = array_pop($request);
if(empty($action)) $action[] = '';

if($action == 'edit')
	$header = "Upravit inzerát";
elseif($action == 'add')
	$header = "Přidat inzerát";
else
	$header = "Neznámá akce";
header_minor($header);
?>
<form action="<?php echo $_SERVER['REQUEST_URI']; ?>" method="POST">
<table>
	<tr>
		<td>Nadpis (*): </td>
		<td><input type="text" name="nadpis" value="<?php echo getPostField("nadpis"); ?>" /></td>
		<td>Max. 255 znaků<?php
			$error = checkPostFieldLength(1, 255, "nadpis") || $error;
		?></td>
	</tr><tr>
		<td>Text: </td>
		<td><textarea rows="15" cols="40" name="text"><?php echo getPostField("text"); ?></textarea></td>
		<td></td>
	</tr><tr>
		<td>Kategorie (*): </td>
		<td>
			<input type="radio" name="kat" value="1" <?php
				if(getPostField("kat") == "1") echo 'checked="checked"';
			?>/>Prodám&nbsp;&nbsp;<input type="radio" name="kat" value="2" <?php
				if(getPostField("kat") == "2") echo 'checked="checked"';
			?>/>Koupím<br/>
			<input type="radio" name="kat" value="3" <?php
				if(getPostField("kat") == "3") echo 'checked="checked"';
			?>/>Hledám partnera&nbsp;&nbsp;<input type="radio" name="kat" value="4" <?php
				if(getPostField("kat") == "4") echo 'checked="checked"';
			?>/>Hledám partnerku<br/>
		</td>
		<td><?php
			$error = checkPostField("/1|2|3|4/i", "kat") || $error;
		?></td>
	</tr><?php if(User::isLogged()) { ?><tr>
		<td>Viditelný: </td>
		<td><input type="checkbox" name="visible" value="visible" <?php
			if(getPostField("visible")) echo 'checked="checked"';
		?>/></td>
		<td></td>
	</tr><?php } else { //TODO: Inzerce foto ?><tr>
		<td>Jméno (*): </td>
		<td><input type="text" name="jmeno" value="<?php echo getPostField("jmeno"); ?>" /></td>
		<td>Max. 40 znaků<?php
			$error = checkPostFieldLength(1, 40, "jmeno") || $error;
		?></td>
	</tr><tr>
		<td>Příjmení (*): </td>
		<td><input type="text" name="prijmeni" value="<?php echo getPostField("prijmeni"); ?>" /></td>
		<td>Max. 40 znaků<?php
			$error = checkPostFieldLength(1, 40, "prijmeni") || $error;
		?></td>
	</tr><?php } ?><tr>
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
		<?php if($action == 'add') { ?>
		<td><button type="submit" name="action" value="add_confirm">Přidat</button></td>
		<?php }	else {?>
		<td><button type="submit" name="action" value="edit_confirm">Upravit</button></td>
		<?php } ?>
		<td></td>
		<td>Pole označená (*) jsou povinná</td>
	</tr>
</table>
</form>
<a href="<?php
echo (strpos($_SERVER['REQUEST_URI'], 'member') ? '/member/profil' : '');
?>/inzerce">Zpět</a>