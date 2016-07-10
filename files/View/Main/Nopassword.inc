<?= $this->header('Zapomenuté heslo') ?>
<?= $this->notice($this->redirect()->getMessage()) ?>

<div class="container full">
    <p>
        Pokud jste zapomněli heslo, pošleme Vám nové na e-mail, který
        jste zadali při registraci.
    </p>

    <form action="" method="post">
        <button type="submit" name="action" value="enter" style="display:none;">Enter</button>
        <table>
            <tr>
                <td>Uživatelské jméno:</td>
                <td><input type="text" name="name" /></td>
            </tr>
            <tr>
                <td>Email:</td>
                <td><input type="text" name="email" /></td>
            </tr>
            <tr>
                <td></td>
                <td><?= $this->submit('Generovat heslo')->data('action', 'gen_pass') ?></td>
            </tr>
        </table>
    </form>
</div>
