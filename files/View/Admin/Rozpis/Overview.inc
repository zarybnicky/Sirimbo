<div class="container">
  <form action="" method="POST">
    <a href="/admin/rozpis/add" class="btn btn-primary">Nový rozpis</a>
    <button class="btn btn-outline-primary" name="action" value="save">Uložit změny</button>
    <?= $this->table()
           ->style('width:100%')
           ->column('buttons', '')
           ->column('links', 'Upravit')
           ->column('fullName', 'Trenér')
           ->column('datum', 'Datum')
           ->column('kde', 'Místo')
           ->column('visible', 'Viditelný')
           ->data($this->data) ?>
  </form>
</div>
