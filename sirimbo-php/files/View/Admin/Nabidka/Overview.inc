<div class="container">
  <form action="" method="POST">
    <a href="/admin/nabidka/add" class="btn btn-primary">Nová nabídka</a>
    <button class="btn btn-outline-primary" name="action" value="save">Uložit změny</button>
    <?= (new TableHelper($this->data))
           ->style('width:100%;')
           ->column('buttons', '')
           ->column('links', 'Upravit')
           ->column('fullName', 'Trenér')
           ->column('date', 'Datum')
           ->column('visible', 'Viditelný') ?>
  </form>
</div>
