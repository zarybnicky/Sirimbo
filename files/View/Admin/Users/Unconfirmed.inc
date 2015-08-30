<?= $this->header('Správa uživatelů') ?>
<div class="container full">
    <h2>Nepotvrzení uživatelé</h2>
    <?= $this->notice($this->redirect()->getMessage()) ?>
    <form action="" method="POST">
        <?= $this->menu()
                 ->content('Potvrdit', 'confirm', true)
                 ->content('Odstranit', 'remove', true)
                 ->content('Zpět', '../users')
        ?>

        <?= $this->table()
                 ->column('checkBox', '')
                 ->column('group', 'Úroveň')
                 ->column('skupina', 'Skupina')
                 ->column('dancer', 'Aktivní<br/>tanečník', 'center')
                 ->column('fullName', 'Jméno', '', 'max-width:140px')
                 ->column('narozeni', 'Datum<br/>narození')
                 ->data($this->data)
        ?>
    </form>
</div>
