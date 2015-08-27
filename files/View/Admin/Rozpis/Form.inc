<?= $this->header('Správa rozpisů') ?>
<?= $this->subheader($this->action == 'add' ? 'Přidat rozpis' : 'Upravit rozpis') ?>
<div class="container full">
<?= $this->notice($this->redirect()->getMessage()) ?>
<form action="" method="POST">
<table>
    <tr>
        <td>Trenér (*): </td>
        <td><?php
            if (!$this->isAdmin) { ?>
            <select name="trener">
                <option value="<?= User::getUserID();?>">Já</option>
            </select><?php
            } else {
                echo $this->userSelect()
                    ->name('trener')
                    ->set($this->trener)
                    ->users(DBUser::getUsersByPermission('rozpis', P_OWNED));
            }?>
        </td>
    </tr><tr>
        <td>Místo (*): </td>
        <td><input type="text" name="kde" value="<?= $this->kde ?>" /></td>
    </tr><tr>
        <td>Datum: (*)</td>
        <td><?= $this->date('datum')->set($this->datum)->textBox();?></td>
    </tr><tr>
        <td>Viditelný: </td>
        <td><label><?php
            if ($this->isAdmin)
                echo $this->checkbox('visible', 'visible')->set($this->visible);
            else
                echo ($this->visible ? '&#10003;' : '&#10799;');
        ?></label></td>
    </tr><tr>
        <td>Uzamknout: </td>
        <td><label><?= $this->checkbox('lock', 'lock')->set($this->lock) ?></label></td>
    </tr><tr>
        <td><button type="submit"><?= $this->action == 'add' ? 'Přidat' : 'Upravit' ?></button></td>
        <td>Pole označená (*) jsou povinná</td>
    </tr>
</table>
</form>
<a href="/admin/rozpis">Zpět</a>
</div>
