<div class="container">
  <div class="center">
    <?= (new TableHelper($data))
           ->style('margin:0 auto;')
           ->column('group', '', 'r')
           ->column('count', 'Počet', 'center big') ?>
    <br/>
    <a class="btn btn-outline-primary" href="/admin/users">Zpět</a>
  </div>
</div>
