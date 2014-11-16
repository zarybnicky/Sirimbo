<?= $this->header('Správa aktualit') ?>
<div class="container full">
<?php
echo '<h2>', ($this->action == 'add' ? 'Přidat článek' : 'Upravit článek'), '</h2>';
echo $this->notice($this->redirect()->getMessage());
?>
<form action="" method="POST">
<table style="width:100%">
    <tr>
        <td>Jméno (*): </td>
        <td>
            <input type="text" name="jmeno" value="<?= post("jmeno"); ?>" style="width:98%"/>
        </td>
        <td class="r">Max. 255 znaků</td>
    </tr><tr>
        <td>Kategorie (*): </td>
        <td>
            <label><?= $this->radio('kat', 1);?>Články</label>&nbsp;
            <label><?= $this->radio('kat', 2);?>Videa</label>&nbsp;
            <label><?= $this->radio('kat', 3);?>Krátké zprávy</label>
        </td>
        <td></td>
    </tr><tr>
        <td>Shrnutí: </td>
        <td></td>
        <td class="r">20 - 50 slov</td>
    </tr><tr>
        <td colspan="3">
            <textarea rows="5" cols="60" name="summary" style="width:99%"><?= post("summary"); ?></textarea>
        </td>
    </tr><tr>
        <td>Text: </td>
        <td></td>
        <td></td>
    </tr><tr>
        <td colspan="3">
            <textarea rows="30" cols="60" name="text" style="width:99%"><?= post("text"); ?></textarea>
        </td>
    </tr><tr>
        <td colspan="2">
            <a href="/admin/aktuality">Zpět</a>
            <?php if ($this->action == 'add'): ?>
                <button type="submit" name="action" value="foto">Přidat fotky</button>
                <button type="submit" name="action" value="save">Přeskočit a uložit</button>
            <?php else: ?>
                <button type="submit">Upravit</button>
            <?php endif; ?>
        </td>
        <td class="r">Pole označená (*) jsou povinná</td>
    </tr>
</table>
</form>
</div>
