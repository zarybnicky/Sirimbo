<?= $this->header('Správa akcí') ?>
<div class="container full">
    <form action="" method="POST">
        <?= $this->render('files/View/Member/Akce/Header.inc', $this->data) ?>
        <?= $this->table()
                 ->data($this->documents)
                 ->column('name', 'Jméno<br/>souboru')
                 ->column('category', 'Kategorie')
                 ->column('removeButton', '') ?>
        <a class="btn btn-outline-primary" href="/admin/akce">Zpět</a>
    </form>
</div>
