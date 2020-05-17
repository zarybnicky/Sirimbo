<div class="container">
  <a class="btn btn-primary" href="/admin/skupiny/add">Přidat skupinu</a>
  <?= $this->table()
         ->style('width:100%')
         ->column('buttons', '', 'center', 'width:50px')
         ->column('colorBox', 'Barvy', 'center', 'width:45px')
         ->column('name', 'Název')
         ->data($this->data) ?>
</div>
