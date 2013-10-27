<h1>Správa akcí</h1>
<?php
echo '<h2>', (Request::getAction() == 'add' ? 'Přidat uživatele' : 'Upravit uživatele'), '</h2>';
notice($this->redirect()->getMessage());
if(isset($f) && is_object($f))
    notice(implode('<br/>', $f->getMessages()));
?>
<form action="" method="POST">
<table>
    <tr>
        <td>Jméno akce (*): </td>
        <td><input type="text" name="jmeno" value="<?php echo post("jmeno"); ?>" /></td>
        <td>Max. 255 znaků</td>
    </tr><tr>
        <td>Místo akce (*): </td>
        <td><input type="text" name="kde" value="<?php echo post("kde"); ?>" /></td>
        <td>Max. 255 znaků</td>
    </tr><tr>
        <td>Další info: </td>
        <td><textarea rows="15" cols="50" name="info"><?php echo post("info"); ?></textarea></td>
        <td></td>
    </tr><tr>
        <td>Od: (*)</td>
        <td><?php echo $this->date('od')->textBox();?></td>
        <td></td>
    </tr><tr>
        <td>Do: </td>
        <td><?php echo $this->date('do')->textBox();?></td>
        <td>(pokud je prázdný, počítá se jako "Od")</td>
    </tr><tr>
        <td>Kapacita (*): </td>
        <td><input type="text" name="kapacita" value="<?php echo post("kapacita"); ?>" /></td>
        <td>Pouze číslice</td>
    </tr><tr>
        <td>Dokumenty: </td>
        <td><?php
            if(!post("dokumenty")) echo "Žádné dokumenty";
            else {
                foreach(post("dokumenty") as $id) {
                    $data = DBDokumenty::getSingleDokument($id);
                    echo "<a href=\"/member/download?id=", $id, "\">", $data["d_name"], "</a>";
                }
            }
        ?></td>
        <td></td>
    </tr><tr>
        <td>Uzamknout: </td>
        <td><label><?php echo getCheckbox('lock', 'lock')?></label></td>
        <td></td>
    </tr><tr>
        <td>Viditelný: </td>
        <td><label><?php echo getCheckbox('visible', 'visible')?></label></td>
        <td></td>
    </tr><tr>
        <td>
            <button type="submit">
                <?php echo (Request::getAction() == 'add') ? 'Přidat' : 'Upravit';?>
            </button>
            <a href="/admin/akce">Zpět</a></td>
        <td>Pole označená (*) jsou povinná</td>
        <td></td>
    </tr>
</table>
</form>