<?= $this->header('Správa akcí') ?>
<div class="container">
    <form action="" method="POST">
        <a href="/admin/akce/add" class="btn btn-primary">Přidat</a>
        <button class="btn btn-primary" name="action" value="save">Uložit změny</button>
        <?= $this->table()
               ->style('width:100%')
               ->column('buttons', '')
               ->column('links', 'Upravit')
               ->column('name', 'Jméno')
               ->column('date', 'Datum')
               ->column('userCount', 'Kapacita')
               ->column('visible', 'Viditelný')
               ->data($this->data) ?>
    </form>
</div>
