<?php
$error = false;

global $isConfirmation;
$isConfirmation = (post("") || post("newpass"));

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
	<td><select name="stt-trida">
			<option value="Z" <?php if(post("stt-trida") == "Z")
				echo "selected=\"selected\"";?>>Z</option>
			<option value="H" <?php if(post("stt-trida") == "H")
				echo "selected=\"selected\"";?>>H</option>
			<option value="D" <?php if(post("stt-trida") == "D")
				echo "selected=\"selected\"";?>>D</option>
			<option value="C" <?php if(post("stt-trida") == "C")
				echo "selected=\"selected\"";?>>C</option>
			<option value="B" <?php if(post("stt-trida") == "B")
				echo "selected=\"selected\"";?>>B</option>
			<option value="A" <?php if(post("stt-trida") == "A")
				echo "selected=\"selected\"";?>>A</option>
			<option value="M" <?php if(post("stt-trida") == "M")
				echo "selected=\"selected\"";?>>M</option>
		</select></td>
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
	<td><select name="lat-trida">
			<option value="Z" <?php if(post("lat-trida") == "Z")
				echo "selected=\"selected\"";?>>Z</option>
			<option value="H" <?php if(post("lat-trida") == "H")
				echo "selected=\"selected\"";?>>H</option>
			<option value="D" <?php if(post("lat-trida") == "D")
				echo "selected=\"selected\"";?>>D</option>
			<option value="C" <?php if(post("lat-trida") == "C")
				echo "selected=\"selected\"";?>>C</option>
			<option value="B" <?php if(post("lat-trida") == "B")
				echo "selected=\"selected\"";?>>B</option>
			<option value="A" <?php if(post("lat-trida") == "A")
				echo "selected=\"selected\"";?>>A</option>
			<option value="M" <?php if(post("lat-trida") == "M")
				echo "selected=\"selected\"";?>>M</option>
		</select></td>
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