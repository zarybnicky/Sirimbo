<?= $this->header('Správa skupin') ?>
<div class="container">
  <a href="/admin/skupiny/add">Přidat skupinu</a>
  <?= $this->table()
         ->style('width:100%')
         ->column('buttons', '', 'center', 'width:50px')
         ->column('colorBox', 'Barvy', 'center', 'width:45px')
         ->column('name', 'Název')
         ->data($this->data) ?>
</div>
