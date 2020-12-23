<div class="container">
    <form action="" method="POST">
        <?php \Render::fragment('files/View/Member/Akce/Header.inc', $data) ?>
        <?= (new \TableHelper($items))
                 ->column('name', 'Uživatel')
                 ->column('removeButton', '') ?>
        <?= new \SubmitHelper('Uložit') ?>
        <a class="btn btn-outline-primary" href="/admin/permissions">Zpět</a>
    </form>
</div>
