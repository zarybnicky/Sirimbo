<div class="container">
    <form action="" method="POST">
        <a href="/admin/aktuality/add" class="btn btn-primary">Nový článek</a>
        <?= $this->table()
             ->style('width:100%')
             ->data($this->data)
             ->column('buttons', '')
             ->column('links', 'Upravit')
             ->column('name', 'Jméno')
             ->column('added', 'Přidán dne') ?>
    </form>
</div>
