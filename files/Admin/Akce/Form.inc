<?php
$error = false;

global $isConfirmation;
$isConfirmation = (post("jmeno") || post("misto"));

if(Request::getAction() == 'edit')
	$header = "Upravit akci";
elseif(Request::getAction() == 'add')
	$header = "Přidat akci";
else
	$header = "Neznámá akce";
header_minor($header);
?>
<form action="<?php echo $_SERVER['REQUEST_URI']; ?>" method="POST">
<table>
	<tr>
		<td>Jméno akce (*): </td>
		<td><input type="text" name="jmeno" value="<?php echo post("jmeno"); ?>" /></td>
		<td>Max. 255 znaků<?php
			$error = checkPostFieldLength(1, 255, "jmeno");
		?><input type="hidden" name="id" value="<?php echo post("id");?>" /></td>
	</tr><tr>
		<td>Místo akce (*): </td>
		<td><input type="text" name="kde" value="<?php echo post("kde"); ?>" /></td>
		<td>Max. 255 znaků<?php
			$error = checkPostFieldLength(1, 255, "kde") || $error;
		?></td>
	</tr><tr>
		<td>Další info: </td>
		<td><textarea rows="15" cols="50" name="info"><?php echo post("info"); ?></textarea></td>
		<td></td>
	</tr><tr>
		<td>Od: (*)</td>
		<td><?php echo Helper::get()->date()->name('od')->textBox();?></td>
		<td><?php $error = checkDateString(Helper::get()->date(false)->getPost()) || $error;?></td>
	</tr><tr>
		<td>Do: </td>
		<td><?php echo Helper::get()->date()->name('do')->textBox();?></td>
		<td>(pokud je prázdný, počítá se jako "Od")
		<?php $do = Helper::get()->date(false)->getPost();
			$error = ($do ? checkDateString($do) : false) || $error;
		?></td>
	</tr><tr>
		<td>Kapacita (*): </td>
		<td><input type="text" name="kapacita" value="<?php echo post("kapacita"); ?>" /></td>
		<td>Pouze číslice<?php
			$error = checkPostField("/^[0-9]{0,11}/", "kapacita") || $error;
		?></td>
	</tr><tr>
		<td>Dokumenty: </td>
		<td><?php
			if(!post("dokumenty")) echo "Žádné dokumenty";
			else {
				foreach(post("dokumenty") as $id) {
					$data = DBDokumenty::getSingleDokument($id);
					echo "<a href=\"/member/download?id=", $id, "\">", $data["d_name"], "</a>";
				}
			}
		?></td>
	</tr><tr>
		<td>Uzamknout: </td>
		<td><label><?php echo getCheckbox('lock', 'lock')?></label></td>
		<td></td>
	</tr><tr>
		<?php if(Request::getAction() == 'add') {?>
		<td><button type="submit" name="action" value="add_confirm">Přidat</button></td>
		<?php }	else {?>
		<td><button type="submit" name="action" value="edit_confirm">Upravit</button></td>
		<?php } ?>
		<td></td>
		<td>Pole označená (*) jsou povinná</td>
	</tr>
</table>
</form>

<a href="/admin/akce">Zpět</a>