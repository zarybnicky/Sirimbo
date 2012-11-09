<?php
$error = false;

global $isConfirmation;
$isConfirmation = (post("stt-trida") || post("lat-trida"));

header_minor("Změna třídy a bodů");
?>
<form action="<?php echo $_SERVER["REQUEST_URI"];?>" method="post">
<table>
<tr><td>Standart:</td><td></td></tr>
<tr>
	<td>Třída:</td>
	<td><?php
		echo Helper::get()->select()
			->post()->name('stt-trida')
			->options(array('Z', 'H', 'D', 'C', 'B', 'A', 'M'), true, true);
	?></td>
</tr>
<tr>
	<td>Body:</td>
	<td><input type="text" name="stt-body" value="<?php echo post('stt-body');?>" /></td>
	<td>0 až 200<?php
		$error = checkPostField("/[012]{0,1}[0-9]{0,2}/", "stt-body") || $error;
	?></td>
</tr>
<tr>
	<td>Finále:</td>
	<td><input type="text" name="stt-finale" value="<?php echo post('stt-finale');?>" /></td>
	<td>0 až 5<?php
		$error = checkPostField("/[012345]{0,1}/", "lat-finale") || $error;
	?></td>
</tr>
<tr><td>Latina:</td><td></td></tr>
<tr>
	<td>Třída:</td>
	<td><?php
		echo Helper::get()->select()
			->post()->name('lat-trida')
			->options(array('Z', 'H', 'D', 'C', 'B', 'A', 'M'), true, true);
	?></td>
</tr>
<tr>
	<td>Body:</td>
	<td><input type="text" name="lat-body" value="<?php echo post('lat-body');?>" /></td>
	<td>0 až 200<?php
		$error = checkPostField("/[0-9]{0,3}/", "lat-body") || $error;
	?></td>
</tr>
<tr>
	<td>Finále:</td>
	<td><input type="text" name="lat-finale" value="<?php echo post('lat-finale');?>" /></td>
	<td>0 až 5<?php
		$error = checkPostField("/[012345]{0,1}/", "lat-finale") || $error;
	?></td>
</tr>
</table>
<button type="submit" name="action" value="edit-class_confirm">Změnit třídu a body</button>
</form>

<a href="/member/profil/par">Zpět</a>