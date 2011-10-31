<form action="<?php echo $_SERVER["REQUEST_URI"] ?>" method="POST">
<div class="a_wrapper">
<div class="a_info" style="border:1px solid black;">
<div class="a_jmeno"><?php echo $akce["a_jmeno"];?></div>
<div class="a_datum"><?php
echo formatDate($akce["a_od"]);
if($akce["a_od"] != $akce["a_do"])
	echo " - ", formatDate($akce["a_do"]);
?>
</div>
<div class="a_moreinfo"><?php echo $akce["a_info"];?></div>
</div>
<table>
<tr><td>Jméno<br/>Souboru</td><td>Kategorie</td><td></td>
</tr>
<?php //TODO: Dokumenty do hlavní Akce 
if(!empty($doku)) {
	$count = count($doku);
	for($i = 0; $i < $count; $i++) {
		$data = DBDokumenty::getSingleDokument($doku[$i]);
		echo '<tr>';
		echo '<td>', $data['d_name'], '</td>';
		echo '<td>', Settings::$document_types[$data['d_kategorie']], '</td>';
		echo '<td><button type="submit" name="remove" value="', $doku[$i],
			'">Odstranit</button></td>';
		echo '</tr>';
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
<a href="/admin/akce">Zpět</a>