<div class="container full">
    <h1>Správa dokumentů</h1>
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
                    <?= $this->submit('Upravit'); ?>
                    <button type="submit" name="action" value="edit_confirm">
                        Upravit
                    </button>
                </td>
            </tr>
        </table>
    </form>
</div>
