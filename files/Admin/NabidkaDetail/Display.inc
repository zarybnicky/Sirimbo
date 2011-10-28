<form action="<?php echo $_SERVER["REQUEST_URI"] ?>" method="POST">
<button type="submit" name="action" value="enter" style="display:none;">Enter</button>
<div class="n_wrapper">
<?php DisplayNabidka::viewNabidkaHeader($nab, $obsazeno); ?>
<table>
<tr><td>Tanečník</td><td>Počet hodin</td></tr>
<?php
if(!empty($items)) {
	foreach($items as $item) {
			echo "<tr>";
			$_POST[$item["ni_id"] . "-partner"] = $item["ni_partner"];
			echo "<td>";
			echoPartners($item["ni_id"] . "-partner", $users);
			echo "</td>";
			echo "<td><input type=\"text\" name=\"", $item["ni_id"], "-hodiny\" value=\"", $item["ni_pocet_hod"],
				"\"/></td>";
			echo "<td><button type=\"submit\" name=\"remove\" value=\"", $item["ni_id"], "\">",
				"Odstranit</button></td>";
			echo "</tr>";
	}
}
?>
<tr>
<td><?php echoPartners("add_partner", $users);?></td>
<td><input type="text" name="add_hodiny" value="" /></td>
<td><button type="submit" name="action" value="add">Přidat</button></td>
</tr>
<tr><td><button type="submit" name="action" value="submit">Uložit</button></td></tr>
</table>
</div>
</form>
<a href="/admin/nabidka">Zpět</a>