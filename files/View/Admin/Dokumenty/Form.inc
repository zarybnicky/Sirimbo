<?= $this->header('Správa dokumentů') ?>
<div class="container full">
    <?= $this->notice($this->redirect()->getMessage()); ?>

    <form action="" method="POST">
        <table>
            <tr>
                <td>Staré jméno:</td>
                <td><?= $this->name ?></td>
            </tr>
            <tr>
                <td>Nové jméno:</td>
                <td><input type="text" name="newname" /></td>
            </tr>
            <tr>
                <td></td>
                <td>
                    <?= $this->submit('Upravit')->data('action', 'edit_confirm') ?>
                </td>
            </tr>
        </table>
    </form>
</div>
