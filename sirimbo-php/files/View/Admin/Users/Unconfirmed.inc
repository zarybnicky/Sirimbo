<div class="container">
  <form action="" method="POST">
    <?= (new TableHelper($data))
           ->style('width:100%')
           ->column('buttons', '')
           ->column('group', 'Úroveň')
           ->column('skupina', 'Skupina')
           ->column('fullName', 'Jméno', '', 'max-width:140px')
           ->column('narozeni', 'Datum<br/>narození')
           ->column('poznamky', 'Poznámka') ?>
  </form>
</div>
