<div class="container">
  <form action="" method="POST">
    <?= $this->table()
           ->style('width:100%')
           ->column('buttons', '')
           ->column('group', 'Úroveň')
           ->column('skupina', 'Skupina')
           ->column('fullName', 'Jméno', '', 'max-width:140px')
           ->column('narozeni', 'Datum<br/>narození')
           ->column('poznamky', 'Poznámka')
           ->data($this->data) ?>
  </form>
</div>
