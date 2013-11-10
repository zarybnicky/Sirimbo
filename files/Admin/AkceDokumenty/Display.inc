<h1>Správa akcí</h1>
<?php
notice($this->redirect()->getMessage());
?>
<form action="" method="POST" class="centerWTable">
<?php echo $this->render('src/application/View/Member/Akce/Header.inc', $this->data);?>
<table>
<tr><td>Jméno<br/>Souboru</td><td>Kategorie</td>
</tr>
<?php
if (!empty($doku)) {
    $count = count($doku);
    for($i = 0; $i < $count; $i++) {
        $data = DBDokumenty::getSingleDokument($doku[$i]);
        echo '<tr>';
        echo '<td>', $data['d_name'], '</td>';
        echo '<td>', Settings::$documentTypes[$data['d_kategorie']], '</td>';
        echo '<td><button type="submit" name="remove" value="', $i,
            '">Odstranit</button></td>';
        echo '</tr>';
    }
}
?>
<tr>
<td><?php
$data = DBDokumenty::getDokumenty();

$select = new \TKOlomouc\View\Helper\Select('add-id');
$select->option('none', '---');
foreach ($data as $item) {
    $select->option($item['d_id'], "{$item['d_name']} ({$item['d_kategorie']})");
}
echo $select;
?></td>
<td><button type="submit" name="add" value="add">Přidat</button></td>
</tr>
</table>
<a href="/admin/akce">Zpět</a>
</form>