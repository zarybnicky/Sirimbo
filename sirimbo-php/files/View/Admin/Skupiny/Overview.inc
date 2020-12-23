<div class="container">
  <a class="btn btn-primary" href="/admin/skupiny/add">Přidat skupinu</a>
  <?= (new TableHelper($data))
         ->style('width:100%')
         ->column('buttons', '', 'center', 'width:70px')
         ->column('colorBox', 'Barvy', 'center', 'width:45px')
         ->column('name', 'Název') ?>
</div>
