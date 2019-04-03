<div class="container">
  <a class="btn btn-primary" href="/admin/video/source/add">Nový zdroj</a>
  <form action="" method="post">
    <?php if ($this->data): ?>
      <?= $this->table()
             ->style('width:100%')
             ->data($this->data)
             ->column('buttons', '', null, 'width:55px')
             ->column('title', 'Jméno')
             ->column('url', 'ID kanálu', null)
             ->column('created', 'Přidáno', null, 'width:100px')
             ->column('lastChecked', 'Poslední kontrola', null, 'width:100px') ?>
    <?php else: ?>
      <?= $this->notice('Žádné zdroje') ?>
    <?php endif ?>
  </form>
</div>
