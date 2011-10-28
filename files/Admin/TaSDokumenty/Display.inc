<form action="<?php echo $_SERVER["REQUEST_URI"] ?>" method="POST">
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
</div>
<table>
<tr><td>Jméno<br/>Souboru</td><td>Kategorie</td><td></td>
</tr>
<?php //TODO: Dokumenty do hlavní TaS 
if(!empty($doku)) {
	$count = count($doku);
	for($i = 0; $i < $count; $i++) {
		$data = DBDokumenty::getSingleDokument($doku[$i]);
		echo "<tr>";
		echo "<td>", $data["d_name"], "</td>";
		echo "<td>", Settings::$document_types[$data["d_kategorie"]], "</td>";
		echo "<td><button type=\"submit\" name=\"remove\" value=\"", $doku[$i],
			"\">Odstranit</button></td>";
		echo "</tr>";
	}
}
?>
<tr>
<td><?php echoTaborDokumenty("add-id", array(2, 3, 0));// = Soutěže, rady, ostatní ?></td>
<td><button type="submit" name="add" value="add">Přidat</button></td>
</tr>
</table>
</div>
</form>
<a href="/admin/tas">Zpět</a>