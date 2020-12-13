<div class="container">
    <form action="" method="POST">
        <a href="/admin/aktuality/add" class="btn btn-primary">Nový článek</a>
        <?= (new TableHelper($this->data))
             ->style('width:100%')
             ->column('buttons', '')
             ->column('links', 'Upravit')
             ->column('name', 'Jméno')
             ->column('added', 'Přidán dne') ?>
    </form>
</div>
