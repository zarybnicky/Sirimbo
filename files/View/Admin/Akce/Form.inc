<?= $this->header('Správa akcí') ?>
<?= $this->subheader($this->header) ?>
<div class="container full">
    <?= $this->notice($this->redirect()->getMessage()); ?>
    <form action="" method="POST">
        <table>
            <tr>
                <td>Jméno akce (*): </td>
                <td>
                    <input type="text" name="jmeno" value="<?= post("jmeno"); ?>" />
                </td>
                <td>Max. 255 znaků</td>
            </tr>
            <tr>
                <td>Místo akce (*): </td>
                <td>
                    <input type="text" name="kde" value="<?= post("kde"); ?>" />
                </td>
                <td>Max. 255 znaků</td>
            </tr>
            <tr>
                <td>Další info: </td>
                <td>
                    <textarea rows="15" cols="50" name="info">
                        <?= post("info"); ?>
                    </textarea>
                </td>
                <td></td>
            </tr>
            <tr>
                <td>Od: (*)</td>
                <td><?= $this->date('od')->textBox();?></td>
                <td></td>
            </tr>
            <tr>
                <td>Do: </td>
                <td><?= $this->date('do')->textBox();?></td>
                <td>(pokud je prázdný, počítá se jako "Od")</td>
            </tr>
            <tr>
                <td>Kapacita (*): </td>
                <td>
                    <input type="text"
                           name="kapacita"
                           value="<?= post("kapacita"); ?>" />
                </td>
                <td>Pouze číslice</td>
            </tr>
            <tr>
                <td>Dokumenty: </td>
                <td>
                    <?php if ($this->dokumenty) : ?>
                        <?php foreach ($this->dokumenty as $item) : ?>
                            <a href="/member/download?id=<?= $item['id'];?>">
                                <?= $item['name'];?>
                            </a>
                        <?php endforeach; ?>
                    <?php else : ?>
                        Žádné dokumenty
                    <?php endif; ?>
                </td>
                <td></td>
            </tr>
            <tr>
                <td>Uzamknout: </td>
                <td>
                    <label><?= $this->checkbox('lock', 'lock')?></label>
                </td>
                <td></td>
            </tr>
            <tr>
                <td>Viditelný: </td>
                <td>
                    <label><?= $this->checkbox('visible', 'visible')?></label>
                </td>
                <td></td>
            </tr>
            <tr>
                <td>
                    <?= $this->submit($this->action) ?>
                    <a href="/admin/akce">Zpět</a></td>
                <td>Pole označená (*) jsou povinná</td>
                <td></td>
            </tr>
        </table>
    </form>
</div>
