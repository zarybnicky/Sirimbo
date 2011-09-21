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
<?php echoNabidky("n_id", DBNabidka::getNabidka()); ?>
<button type="submit" name="action" value="select">Vybrat nabídku</button>
<?php
if(getPostField("n_id")) {				//BEGIN IF
	if(!is_numeric(getPostField("n_id")) && getPostField("n_id") > 0 && getPostField("n_id") < 2147483647)
		View::viewError(ER_CORRUPT_DATA);
	if(!($nab = DBNabidka::getSingleNabidka(getPostField("n_id"))))
		View::viewError(ER_CORRUPT_DATA);
	$items = DBNabidka::getNabidkaItem(getPostField("n_id"));
	$obsazeno = DBNabidka::getNabidkaItemLessons(getPostField("n_id"));
?>
<div class="n_wrapper">
<div class="n_info" style="border:1px solid black;">
<div class="n_trener"><?php echo $nab["u_jmeno"], " ", $nab["u_prijmeni"]; ?></div>
<div class="n_datum"><?php
echo formatDate($nab["n_od"]);
if($nab["n_od"] != $nab["n_do"])
	echo " - ", formatDate($nab["n_do"]);
?></div>
<div class="n_hodiny_all">Celkem hodin: <?php echo $nab["n_pocet_hod"];?></div>
<div class="n_hodiny_obs">Obsazených hodin: <?php echo $obsazeno;?></div>
<div class="n_hodiny_vol">Volných hodin: <?php echo $nab["n_pocet_hod"] - $obsazeno;?></div>
</div>
<table>
<tr><td>Tanečník</td><td>Počet hodin</td></tr>
<?php
if(!empty($items)) {
	foreach($items as $item) {
			echo "<tr>";
			echo "<td>", DBUser::getUserWholeName($item["ni_partner"]), "</td>";
			echo "<td>", $item["ni_pocet_hod"], "</td>";
			echo "</tr>";
	}
}
?>
</table>
</div>
<?php
}										//END IF
?>
<br/><br/>
Pomocník ((?))<br/>
<button type="submit" name="generate" value="overlap">Opravit přesahy</button><br/>
<button type="submit" name="generate" value="gen_add">Přidat</button>
<input type="text" name="gen_add_hod" value="" /> hodin po
<input type="text" name="gen_add_len" value="" /> minutách od
<input type="text" name="gen_add_od" value="" />
</form>

<form action="/admin/rozpis" method="GET">
<button type="submit">Zpět</button>
</form>