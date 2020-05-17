<div class="container">
  <form action="" method="POST">
    <a href="/admin/permissions/add" class="btn btn-primary">Přidat</a>
    <?= $this->table()
           ->style('width:100%')
           ->data($this->data)
           ->column('buttons', '')
           ->column('name', 'Jméno')
           ->column('description', 'Popis') ?>
  </form>
</div>
