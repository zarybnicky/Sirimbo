<form action="<?php echo $_SERVER["REQUEST_URI"] ?>" method="POST">
<input type="submit" name="action" value="enter" style="display:none;" />
<div class="n_wrapper">
<div class="n_info" style="border:1px solid black;">
<div class="n_trener"><?php echo $nab["u_jmeno"], " ", $nab["u_prijmeni"]; ?></div>
<div class="n_datum"><?php
echo formatDate($nab["n_od"]);
if($nab["n_od"] != $nab["n_do"])
	echo " - ", formatDate($nab["n_do"]);
?></div>
<div class="n_hodiny_all">Celkem hodin: <input type="text" name="pocet_hod" value="<?php
	echo $nab["n_pocet_hod"]; ?>" /></div>
<div class="n_hodiny_obs">Obsazených hodin: <?php echo $obsazeno;?></div>
<div class="n_hodiny_vol">Volných hodin: <?php echo $nab["n_pocet_hod"] - $obsazeno;?></div>
</div>
<table>
<tr><td>Tanečník</td><td>Počet hodin</td></tr>
<?php
if(!empty($items)) {
	foreach($items as $item) {
			echo "<tr>";
			$_POST[$item["ni_id"] . "-partner"] = $item["ni_partner"];
			echo "<td>";
			echoUsers($item["ni_id"] . "-partner", $users);
			echo "</td>";
			echo "<td><input type=\"text\" name=\"", $item["ni_id"], "-hodiny\" value=\"", $item["ni_pocet_hod"],
				"\"/></td>";
			echo "<td><button type=\"submit\" name=\"remove\" value=\"", $item["ni_id"], "\">",
				"Odstranit</button></td>";
			echo "</tr>";
	}
}
//FIXME: ošetřit - "simple" edit nabídky - max hodin vs počet hodin
?>
<tr>
<td><?php echoUsers("add_partner", $users);?></td>
<td><input type="text" name="add_hodiny" value="" /></td>
<td><button type="submit" name="action" value="add">Přidat</button></td>
</tr></table>
</div>
</form>