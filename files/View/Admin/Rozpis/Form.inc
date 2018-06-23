<?= $this->header(
    'Správa rozpisů',
    $this->action == 'add' ? 'Přidat rozpis' : 'Upravit rozpis'
) ?>

<div class="container full">
    <form action="" method="POST">
        <table>
            <tr>
                <td>Trenér (*): </td>
                <td><?= $this->userSelect('trener')
                             ->set($this->trener)
                             ->tmpSwitch($this->isAdmin)
                             ->users($this->treneri) ?>
                </td>
            </tr><tr>
                <td>Místo (*): </td>
                <td><?= $this->text('kde', $this->kde) ?></td>
            </tr><tr>
                <td>Datum: (*)</td>
                <td><?= $this->date('datum', $this->datum)->textBox() ?></td>
            </tr><tr>
                <td>Viditelný: </td>
                <td><?= $this->checkbox('visible', 'visible')->set($this->visible) ?></td>
            </tr><tr>
                <td>Uzamknout: </td>
                <td><?= $this->checkbox('lock', 'lock')->set($this->lock) ?></td>
            </tr><tr>
                <td><?= $this->submit($this->action == 'add' ? 'Přidat' : 'Upravit') ?></td>
                <td>Pole označená (*) jsou povinná</td>
            </tr>
        </table>
    </form>
    <a href="/admin/rozpis">Zpět</a>
</div>
