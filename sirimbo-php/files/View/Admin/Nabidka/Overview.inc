<div class="container">
  <form action="" method="POST">
    <a href="/admin/nabidka/add" class="btn btn-primary">Nová nabídka</a>
    <button class="btn btn-outline-primary" name="action" value="save">Uložit změny</button>
    <?= $this->table()
           ->style('width:100%;')
           ->data($this->data)
           ->column('buttons', '')
           ->column('links', 'Upravit')
           ->column('fullName', 'Trenér')
           ->column('date', 'Datum')
           ->column('visible', 'Viditelný') ?>
  </form>
</div>
