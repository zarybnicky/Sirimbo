<h1>Správa dokumentů</h1>
<?php
echo $this->notice($this->redirect()->getMessage());

if (isset($this->form)) {
    echo '<form action="" method="POST">';
    echo 'Staré jméno:&nbsp;', $this->form['d_name'], '<br />';
    echo 'Nové jméno:&nbsp;';
    echo '<input type="text" name="newname" value="', $this->form['d_name'], '" />';
    echo '<button type="submit" name="action" value="edit_confirm">Upravit</button>';
    echo '</form><br />';
}

?>
<form action="/admin/dokumenty" method="POST">
<button type="submit" name="action" value="edit">Přejmenovat</button>
<button type="submit" name="action" value="remove">Odstranit</button>
<table>
<tr>
<td></td>
<td>Jméno souboru</td>
<td>Soubor</td>
<td>Kategorie</td>
<td>Uploadoval</td>
</tr>
<?php
$dokumenty = DBDokumenty::getDokumenty();
foreach ($dokumenty as $item) {
    echo "<tr>";
    if (Permissions::check('dokumenty', P_OWNED, $item['d_kdo']))
        echo '<td><input type="checkbox" name="dokumenty[]" value="', $item['d_id'],
            '" /></td>';
    else
        echo '<td><input type="checkbox" name="dokumenty[]" value="',
            '" disabled="disabled" /></td>';
    echo '<td><a href="/member/download?id=', $item['d_id'], '">', $item['d_name'], '</a></td>';
    echo '<td>', $item['d_filename'], '</td>';
    echo '<td>', Settings::$documentTypes[$item['d_kategorie']], '</td>';
    echo '<td>';
    echoFullJmeno($item);
    echo '</td>';
    echo '</tr>';
}
?>
</table>
</form>
<hr />
<h2>Upload</h2>
<form action="" method="post" enctype="multipart/form-data">
<table>
<tr><td>Jméno:</td><td><input type="text" name="name" /></td></tr>
<tr><td>Soubor:</td><td><input type="file" name="file" /></td></tr>
<tr><td>Kategorie</td><td><select name="kategorie">
    <option value="1">Schůze, rady</option>
    <option value="2">Soutěže</option>
    <option value="3">Tábory</option>
    <option value="0">Ostatní</option>
</select></td></tr>
<tr><td></td><td><button type="submit" name="action" value="upload">Upload</button></td></tr>
</table>
</form>
