<?= $this->header('Správa nabídky') ?>
<?= $this->subheader(($this->action == 'add' ? 'Přidat' : 'Upravit') . ' nabídku') ?>
<div class="container full">
    <?= $this->notice($this->redirect()->getMessage()); ?>

    <form action="" method="POST">
        <button type="submit" style="display:none;">Enter</button>

        <table>
            <tr>
                <td>Trenér (*):</td>
                <td>
                    <?php if (!Permissions::check('nabidka', P_ADMIN)) : ?>
                        <select name="trener">
                            <option value="<?= User::getUserID();?>">Já</option>
                        </select>
                    <?php else : ?>
                        <?= $this->userSelect()
                                 ->name('trener')
                                 ->users($this->users); ?>
                    <?php endif; ?>
                </td>
            </tr>
            <tr>
                <td>Počet hodin (*):</td>
                <td>
                    <input type="text"
                           name="pocet_hod"
                           value="<?= post("pocet_hod"); ?>" />
                </td>
            </tr>
            <tr>
                <td>
                    Max. počet hodin/pár (*):
                </td>
                <td>
                    <input type="text"
                           name="max_pocet_hod"
                           value="<?= post("max_pocet_hod"); ?>" />
                </td>
            </tr>
            <tr>
                <td></td>
                <td>(0 = neomezeně)</td>
            </tr>
            <tr>
                <td>Od: (*)</td>
                <td><?= $this->date('od')->textBox();?></td>
            </tr>
            <tr>
                <td>Do: </td>
                <td><?= $this->date('do')->textBox();?></td>
            </tr>
            <tr>
                <td></td>
                <td>(pokud je "Do" prázdné, počítá se jako "Od")</td>
            </tr>
            <tr>
                <td>Viditelný: </td>
                <td>
                    <label>
                        <?= (Permissions::check('nabidka', P_ADMIN) ?
                             $this->checkbox('visible', 'visible') :
                             ($this->visible ?
                              '&#10003;' :
                              '&#10799;'));?>
                    </label>
                </td>
            </tr>
            <tr>
                <td>Uzamknout: </td>
                <td>
                    <label>
                        <?= $this->checkbox('lock');?>
                    </label>
                </td>
            </tr>
            <tr>
                <td>
                    <input type="hidden"
                           name="referer"
                           value="<?= $this->referer ?>" />
                    <?= $this->submit(($this->action == 'add') ?
                                       'Přidat' :
                                       'Upravit'); ?>
                    </button>
                </td>
                <td>Pole označená (*) jsou povinná</td>
            </tr>
        </table>
    </form>
    <a href="<?= $this->returnURI;?>">Zpět</a>
</div>
