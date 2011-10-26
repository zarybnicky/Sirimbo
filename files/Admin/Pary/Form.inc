<?php
$error = false;

global $isConfirmation;
$isConfirmation = (getPostField("") || getPostField("newpass"));

header_minor("Změna třídy a bodů");
?>
<form action="<?php echo $_SERVER["REQUEST_URI"];?>" method="post">
Pár: <?php echo $data['guy_name'], ' ', $data['guy_surname'], ' - ',
	$data['gal_name'], ' ', $data['gal_surname'];?>
<input type="hidden" name="id" value="<?php echo $data['p_id'];?>" />
<table>
<tr><td>Standart:</td><td></td></tr>
<tr>
	<td>Třída:</td>
	<td><select name="stt-trida">
			<option value="H" <?php if(getPostField("stt-trida") == "H")
				echo "selected=\"selected\"";?>>H</option>
			<option value="D" <?php if(getPostField("stt-trida") == "D")
				echo "selected=\"selected\"";?>>D</option>
			<option value="C" <?php if(getPostField("stt-trida") == "C")
				echo "selected=\"selected\"";?>>C</option>
			<option value="B" <?php if(getPostField("stt-trida") == "B")
				echo "selected=\"selected\"";?>>B</option>
			<option value="A" <?php if(getPostField("stt-trida") == "A")
				echo "selected=\"selected\"";?>>A</option>
			<option value="M" <?php if(getPostField("stt-trida") == "M")
				echo "selected=\"selected\"";?>>M</option>
		</select></td>
</tr>
<tr>
	<td>Body:</td>
	<td><input type="text" name="stt-body" value="<?php echo getPostField('stt-body');?>" /></td>
	<td>0 až 200<?php
		$error = checkPostField("/[012]{0,1}[0-9]{0,2}/", "stt-body") || $error;
	?></td>
</tr>
<tr>
	<td>Finále:</td>
	<td><input type="text" name="stt-finale" value="<?php echo getPostField('stt-finale');?>" /></td>
	<td>0 až 5<?php
		$error = checkPostField("/[012345]{0,1}/", "lat-finale") || $error;
	?></td>
</tr>
<tr><td>Latina:</td><td></td></tr>
<tr>
	<td>Třída:</td>
	<td><select name="lat-trida">
			<option value="H" <?php if(getPostField("lat-trida") == "H")
				echo "selected=\"selected\"";?>>H</option>
			<option value="D" <?php if(getPostField("lat-trida") == "D")
				echo "selected=\"selected\"";?>>D</option>
			<option value="C" <?php if(getPostField("lat-trida") == "C")
				echo "selected=\"selected\"";?>>C</option>
			<option value="B" <?php if(getPostField("lat-trida") == "B")
				echo "selected=\"selected\"";?>>B</option>
			<option value="A" <?php if(getPostField("lat-trida") == "A")
				echo "selected=\"selected\"";?>>A</option>
			<option value="M" <?php if(getPostField("lat-trida") == "M")
				echo "selected=\"selected\"";?>>M</option>
		</select></td>
</tr>
<tr>
	<td>Body:</td>
	<td><input type="text" name="lat-body" value="<?php echo getPostField('lat-body');?>" /></td>
	<td>0 až 200<?php
		$error = checkPostField("/[0-9]{0,3}/", "lat-body") || $error;
	?></td>
</tr>
<tr>
	<td>Finále:</td>
	<td><input type="text" name="lat-finale" value="<?php echo getPostField('lat-finale');?>" /></td>
	<td>0 až 5<?php
		$error = checkPostField("/[012345]{0,1}/", "lat-finale") || $error;
	?></td>
</tr>
</table>
	<button type="submit" name="action" value="edit_confirm">Změnit třídu a body</button>
</form>

<form action="<?php echo $_SERVER["REQUEST_URI"] ?>" method="GET">
<button type="submit">Zpět</button>
</form>