<div class="container">
  <div class="center">
    <?= $this->table()
           ->style('margin:0 auto;')
           ->column('group', '', 'r')
           ->column('count', 'Počet', 'center big')
           ->data($this->data) ?>
    <br/>
    <a class="btn btn-outline-primary" href="/admin/users">Zpět</a>
  </div>
</div>
