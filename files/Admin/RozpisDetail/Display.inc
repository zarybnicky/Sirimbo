<form action="<?php echo $_SERVER["REQUEST_URI"] ?>" method="POST">
<input type="submit" name="action" value="enter" style="display:none;" />
<div class="r_wrapper">
<div class="r_info" style="border:1px solid black;">
<div class="r_datum"><?php echo formatDate($roz["r_datum"]);?></div>
<div class="r_kde"><?php echo $roz["r_kde"];?></div>
<div class="r_trener"><?php echo $roz["u_jmeno"], " ", $roz["u_prijmeni"];?></div>
</div>
<table>
<tr><td>Tanečník</td><td>Od</td><td>Do</td><td></td><td>Zámek</td></tr>
<?php
if(!empty($items)) {
	foreach($items as $item) {
		echo "<tr>";
		echo "<td>";
		$_POST[$item["ri_id"] . "-partner"] = $item["ri_partner"];
		echoUsers($item["ri_id"] . "-partner", $users);
		echo "</td>";
		echo "<td><input type=\"text\" name=\"", $item["ri_id"], "-od\" value=\"",
			formatTime($item["ri_od"], 1), "\"/></td>";
		echo "<td><input type=\"text\" name=\"", $item["ri_id"], "-do\" value=\"",
			formatTime($item["ri_do"], 1), "\"/></td>";
		echo "<td><button type=\"submit\" name=\"remove\" value=\"", $item["ri_id"],
			"\">Odstranit</button></td>";
		echo "<td><input type=\"checkbox\" name=\"", $item["ri_id"],
			"-lock\" value=\"lock\"";
		if($item["ri_lock"]) echo "checked=\"checked\"";
		echo " /></td>";
		echo "</tr>";
	}
}
?>
<tr>
<td><?php echoUsers("add_partner", $users);?></td>
<td><input type="text" name="add_od" value="" /></td>
<td><input type="text" name="add_do" value="" /></td>
<td><button type="submit" name="action" value="add">Přidat</button></td>
</tr>
</table>
</div>
<br/>
Pomocník ((?))<br/>
<button type="submit" name="generate" value="overlap">Opravit přesahy</button><br/>
<button type="submit" name="generate" value="gen_add">Přidat</button>
<input type="text" name="gen_add_hod" value="" /> hodin po
<input type="text" name="gen_add_len" value="" /> minutách od
<input type="text" name="gen_add_od" value="" />
</form>