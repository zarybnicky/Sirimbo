<h1>Správa aktualit</h1>
<?php
echo '<h2>', ($this->action == 'add' ? 'Přidat článek' : 'Upravit článek'), '</h2>';
notice($this->redirect()->getMessage());
?>
<form action="" method="POST">
<table>
    <tr>
        <td>Jméno (*): </td>
        <td><input type="text" name="jmeno" value="<?php echo post("jmeno"); ?>" /></td>
        <td>Max. 255 znaků</td>
    </tr>
    <tr>
        <td>Text: </td>
        <td><textarea rows="15" cols="40" name="text"><?php echo post("text"); ?></textarea></td>
        <td></td>
    </tr>
    <tr>
        <td>Kategorie (*): </td>
        <td>
            <label><?php echo getRadio('kat', 1);?>Články</label>&nbsp;
            <label><?php echo getRadio('kat', 2);?>Videa</label>&nbsp;
            <label><?php echo getRadio('kat', 3);?>Krátké zprávy</label>
        </td>
        <td></td>
    </tr>
    <tr>
        <td>Fotogalerie: </td>
        <td><?php
            $s = $this->select()
                ->post()->name('foto')
                ->value(post('foto'))
                ->option('none', '---');
            foreach ($this->fotoDirs as $item) {
                $s->option($dir['id'], $item['name']);
            }
            echo $s;
        ?></td>
        <td></td>
    </tr>
    <tr>
        <td>
            <button type="submit">
                <?= ($this->action == 'add') ? 'Přidat' : 'Upravit'; ?>
            </button>
        </td>
        <td>Pole označená (*) jsou povinná</td>
        <td></td>
    </tr>
</table>
</form>
<a href="/admin/aktuality">Zpět</a>