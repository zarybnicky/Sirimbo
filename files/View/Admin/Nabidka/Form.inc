<?= $this->header('Správa nabídky', $this->header) ?>
<div class="container full">
    <?= $this->notice($this->redirect()->getMessage()); ?>

    <form action="" method="POST">
        <button type="submit" style="display:none;">Enter</button>

        <table>
            <tr>
                <td>Trenér (*):</td>
                <td>
                    <?= $this->userSelect('trener')
                             ->users($this->users)
                             ->set($this->trener)
                             ->tmpSwitch($this->isAdmin)
                             ->render() ?>
                </td>
            </tr>
            <tr>
                <td>Počet hodin (*):</td>
                <td><?= $this->text('pocet_hod', $this->pocet_hod) ?></td>
            </tr>
            <tr>
                <td>Max. počet hodin/pár (*):</td>
                <td><?= $this->text('max_pocet_hod', $this->max_pocet_hod) ?></td>
            </tr>
            <tr>
                <td></td>
                <td>(0 = neomezeně)</td>
            </tr>
            <tr>
                <td>Od: (*)</td>
                <td><?= $this->date('od', $this->od)->textBox() ?></td>
            </tr>
            <tr>
                <td>Do: </td>
                <td><?= $this->date('do', $this->do)->textBox() ?></td>
            </tr>
            <tr>
                <td></td>
                <td>(pokud je "Do" prázdné, počítá se jako "Od")</td>
            </tr>
            <tr>
                <td>Viditelný: </td>
                <td>
                    <?= $this->isAdmin
                        ? $this->checkbox('visible', 'visible')->set($this->visible)
                        : ($this->visible ? '&#10003;' : '&#10799;') ?>
                </td>
            </tr>
            <tr>
                <td>Uzamknout: </td>
                <td><?= $this->checkbox('lock')->set($this->lock) ?></td>
            </tr>
            <tr>
                <td>
                    <?= $this->hidden('referef', $this->referer) ?>
                    <?= $this->submit($this->action) ?>
                    </button>
                </td>
                <td>Pole označená (*) jsou povinná</td>
            </tr>
        </table>
    </form>
    <a href="<?= $this->referer ?>">Zpět</a>
</div>
