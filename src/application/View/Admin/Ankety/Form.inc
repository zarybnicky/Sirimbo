<h1>Správa anket</h1>
<?php
echo '<h2>', ($this->action == 'add' ? 'Přidat anketu' : 'Upravit anketu'), '</h2>';
notice($this->redirect()->getMessage());
?>
<form action="" method="post">
<button type="submit" name="action" value="enter" style="display:none;">Enter</button>
<table>
    <tr>
        <td>Jméno: </td>
        <td><input type="text" name="jmeno" value="<?php echo post('jmeno');?>" /></td>
    </tr>
    <tr>
        <td>Text: </td>
        <td><input type="text" name="text" value="<?php echo post('text');?>" /></td>
    </tr><tr>
        <td>Viditelný: </td>
        <td>
            <?php echo getCheckbox('visible', 'visible', false, false, $this->canMakeVisible);?>
        </td>
    </tr><tr>
        <td>Text</td><td>Počet hlasů</td>
    </tr>
    <?php foreach ($items as $item) : ?>
    <tr>
        <td>
            <input type="text" name="<?= $item['id'] ?>-text" value="<?= $item['text'] ?>" />
        </td>
        <td>
            <?= $item['pocet'] ?>
        </td>
        <td>
            <button type="submit" name="remove" value="<?= $item['id'] ?>">
                Odstranit a uložit
            </button>
        </td>
    </tr>
    <?php endforeach; ?>
    <tr>
        <td><input type="text" name="add_text" /></td>
        <td>0</td>
        <td><button type="submit" name="action" value="confirm">Přidat a uložit</button></td>
    </tr><tr>
        <td><button type="submit" name="action" value="confirm">Uložit</button></td>
        <td><a href="/admin/ankety">Zpět</a></td>
    </tr>
</table>
</form>