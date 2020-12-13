<div class="container">
    <form action="" method="POST">
        <a href="/admin/akce/add" class="btn btn-primary">Přidat</a>
        <button class="btn btn-primary" name="action" value="save">Uložit změny</button>
        <?= (new TableHelper($this->data))
               ->style('width:100%')
               ->column('buttons', '')
               ->column('links', 'Upravit')
               ->column('name', 'Jméno')
               ->column('date', 'Datum')
               ->column('userCount', 'Kapacita')
               ->column('visible', 'Viditelný') ?>
    </form>
</div>
