<?= $this->header('Správa akcí') ?>
<div class="container">
    <form action="" method="POST">
        <?= $this->render('files/View/Member/Akce/Header.inc', $this->data);?>
        <?= $this->table()
                 ->data($this->items)
                 ->column('name', 'Uživatel')
                 ->column('removeButton', '') ?>
        <?= $this->submit('Uložit') ?>
        <a class="btn btn-outline-primary" href="/admin/permissions">Zpět</a>
    </form>
</div>
