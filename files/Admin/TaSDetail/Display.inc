<form action="<?php echo $_SERVER["REQUEST_URI"] ?>" method="POST">
<button type="submit" name="action" value="enter" style="display:none;">Enter</button>
<div class="t_wrapper">
<div class="t_info" style="border:1px solid black;">
<div class="t_jmeno"><?php echo $tas["t_jmeno"];?></div>
<div class="t_datum"><?php
echo formatDate($tas["t_od"]);
if($tas["t_od"] != $tas["t_do"])
	echo " - ", formatDate($tas["t_do"]);
?>
</div>
<div class="t_moreinfo"><?php echo $tas["t_info"];?></div>
<div class="t_dokumenty"><?php
$dokumekty = $tas["t_dokumenty"];
if(!empty($dokumenty))
	foreach(unserialize($tas["t_dokumenty"]) as $doku) {
		echo DBDokumenty::getDokumentName($doku), " ";
	}
?></div>
</div>
<table>
<tr>
<td>Registrovaný<br/>uživatel</td><td>Uživatel</td>
<td>Jméno</td><td>Příjmení</td><td>Rok narození</td><td></td>
</tr>
<?php
if(!empty($items)) {
	foreach($items as $item) {
		echo "<tr>";
		echo "<td><input type=\"checkbox\" name=\"", $item["ti_id"], "-reg\"";
		if($item["ti_user"] > 0)
			echo "checked=\"checked\"";
		echo " /></td>";
		echo "<td>";
		if($item["ti_user"] > 0)
			$_POST[$item["ti_id"] . "-user"] = $item["ti_user"];
		echoUsers($item["ti_id"] . "-user", $users);
		echo "</td>";
		echo "<td><input type=\"text\" name=\"", $item["ti_id"], "-jmeno\" value=\"",
			$item["ti_jmeno"], "\" /></td>";
		echo "<td><input type=\"text\" name=\"", $item["ti_id"], "-prijmeni\" value=\"",
			$item["ti_prijmeni"], "\" /></td>";
		echo "<td><input type=\"text\" name=\"", $item["ti_id"], "-rok\" value=\"",
			$item["ti_rok_narozeni"], "\" /></td>";
		echo "<td><button type=\"submit\" name=\"remove\" value=\"", $item["ti_id"],
			"\">Odstranit</button></td>";
		echo "</tr>";
	}
}
?>
<tr>
<td><input type="checkbox" name="add-reg" /></td>
<td><?php echoUsers("add-user", $users);?></td>
<td><input type="text" name="add-jmeno" /></td>
<td><input type="text" name="add-prijmeni" /></td>
<td><input type="text" name="add-rok" /></td>
<td><button type="submit" name="add" value="add">Přidat</button></td>
</tr>
<tr><td><button type="submit" name="action" value="submit">Uložit</button></td></tr>
</table>
</div>
</form>
<a href="/admin/tas">Zpět</a>