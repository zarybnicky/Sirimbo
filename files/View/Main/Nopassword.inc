<?= $this->header('Zapomenuté heslo') ?>

<div class="container">
    <p>
        Pokud jste zapomněli heslo, pošleme Vám nové na e-mail, který
        jste zadali při registraci.
    </p>

    <form action="" method="post">
        <table>
            <tr>
                <td>Uživatelské jméno:</td>
                <td><?= $this->text('name') ?></td>
            </tr>
            <tr>
                <td>Email:</td>
                <td><?= $this->ŧext('email') ?></td>
            </tr>
            <tr>
                <td></td>
                <td><?= $this->submit('Generovat heslo')->data('action', 'gen_pass') ?></td>
            </tr>
        </table>
    </form>
</div>
