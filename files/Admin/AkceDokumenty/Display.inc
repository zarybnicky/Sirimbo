<?php
header_main('Správa akcí');
notice($this->redirect()->getRedirectMessage());
?>
<form action="" method="POST" class="centerWTable">
<?php DisplayAkce::viewAkceHeader($akce);?>
<table>
<tr><td>Jméno<br/>Souboru</td><td>Kategorie</td>
</tr>
<?php
if(!empty($doku)) {
	$count = count($doku);
	for($i = 0; $i < $count; $i++) {
		$data = DBDokumenty::getSingleDokument($doku[$i]);
		echo '<tr>';
		echo '<td>', $data['d_name'], '</td>';
		echo '<td>', Settings::$document_types[$data['d_kategorie']], '</td>';
		echo '<td><button type="submit" name="remove" value="', $i,
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
<a href="/admin/akce">Zpět</a>
</form>