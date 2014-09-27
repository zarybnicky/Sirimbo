<h1>Správa akcí</h1>
<?php
echo '<h2>';
echo Request::getAction() == 'add' ? 'Přidat uživatele' : 'Upravit uživatele';
echo '</h2>';
notice($this->redirect()->getMessage());
if ($this->form) {
    notice(implode('<br/>', $this->form->getMessages()));
}
?>
<form action="" method="POST">
<table>
    <tr>
        <td>Jméno akce (*): </td>
        <td>
            <input type="text" name="jmeno" value="<?php echo post("jmeno"); ?>" />
        </td>
        <td>Max. 255 znaků</td>
    </tr><tr>
        <td>Místo akce (*): </td>
        <td>
            <input type="text" name="kde" value="<?php echo post("kde"); ?>" />
        </td>
        <td>Max. 255 znaků</td>
    </tr><tr>
        <td>Další info: </td>
        <td>
            <textarea rows="15" cols="50" name="info"><?php echo post("info"); ?>
            </textarea>
        </td>
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
    if (!$this->dokumenty) :
            ?>Žádné dokumenty<?php
    else :
        foreach ($this->dokumenty as $item) :
            ?><a href="/member/download?id=<?php echo $item['id'];?>">
                <?php echo $item['name'];?>
            </a><?php
        endforeach;
    endif;
        ?></td>
        <td></td>
    </tr><tr>
        <td>Uzamknout: </td>
        <td><label><?php echo $this->checkbox('lock', 'lock')?></label></td>
        <td></td>
    </tr><tr>
        <td>Viditelný: </td>
        <td><label><?php echo $this->checkbox('visible', 'visible')?></label></td>
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
