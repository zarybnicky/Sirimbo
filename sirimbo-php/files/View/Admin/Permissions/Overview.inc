<div class="container">
  <form action="" method="POST">
    <a href="/admin/permissions/add" class="btn btn-primary">Přidat</a>
    <?= (new TableHelper($this->data))
           ->style('width:100%')
           ->column('buttons', '')
           ->column('name', 'Jméno')
           ->column('description', 'Popis') ?>
  </form>
</div>
