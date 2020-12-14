<div class="container">
    <form action="" method="POST">
        <?= new \RenderHelper('files/View/Member/Akce/Header.inc', $this->data) ?>
        <?= (new TableHelper($this->items))
                 ->column('name', 'Uživatel')
                 ->column('removeButton', '') ?>
        <?= new \SubmitHelper('Uložit') ?>
        <a class="btn btn-outline-primary" href="/admin/permissions">Zpět</a>
    </form>
</div>
