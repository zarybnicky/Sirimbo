<?php
$error = false;

global $isConfirmation;
$isConfirmation = (post('stt-trida') || post("lat-trida"));

header_minor("Změna třídy a bodů");
?>
<form action="<?php echo $_SERVER['REQUEST_URI']; ?>" method="POST">
Pár: <?php echo $data['guy_name'], ' ', $data['guy_surname'], ' - ',
	$data['gal_name'], ' ', $data['gal_surname'];?>
<input type="hidden" name="id" value="<?php echo $data['p_id'];?>" />
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
</tr>
<tr><td>Finále:</td>
	<td><input type="text" name="stt-finale" value="<?php echo post('stt-finale');?>" /></td>
</tr>
<tr><td>Latina:</td><td></td></tr>
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
	<td><input type="text" name="lat-body" value="<?php echo post('lat-body');?>" /></td>
</tr>
<tr>
	<td>Finále:</td>
	<td><input type="text" name="lat-finale" value="<?php echo post('lat-finale');?>" /></td>
</tr>
</table>
	<button type="submit" name="action" value="edit_confirm">Změnit třídu a body</button>
</form>

<a href="/admin/pary">Zpět</a>