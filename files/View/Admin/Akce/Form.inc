<?= $this->header('Správa akcí', $this->header) ?>
<div class="container full">
    <?= $this->notice($this->redirect()->getMessage()); ?>
    <form action="" method="POST">
        <table>
            <tr>
                <td>Jméno akce (*): </td>
                <td>
                    <input type="text" name="jmeno" value="<?= $this->jmeno ?>" />
                </td>
                <td>Max. 255 znaků</td>
            </tr>
            <tr>
                <td>Místo akce (*): </td>
                <td>
                    <input type="text" name="kde" value="<?= $this->kde ?>" />
                </td>
                <td>Max. 255 znaků</td>
            </tr>
            <tr>
                <td>Další info: </td>
                <td>
                    <textarea rows="15"
                              cols="50"
                              name="info"><?= $this->info ?></textarea>
                </td>
                <td></td>
            </tr>
            <tr>
                <td>Od: (*)</td>
                <td><?= $this->date('od', $this->od)->textBox() ?></td>
                <td></td>
            </tr>
            <tr>
                <td>Do: </td>
                <td><?= $this->date('do', $this->do)->textBox() ?></td>
                <td>(pokud je prázdný, počítá se jako "Od")</td>
            </tr>
            <tr>
                <td>Kapacita (*): </td>
                <td>
                    <input type="text"
                           name="kapacita"
                           value="<?= $this->kapacita ?>" />
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
                    <?= $this->checkbox('lock', 'lock')->set($this->lock) ?>
                </td>
                <td></td>
            </tr>
            <tr>
                <td>Viditelný: </td>
                <td>
                    <?= $this->checkbox('visible', 'visible')->set($this->visible) ?>
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
