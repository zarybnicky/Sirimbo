<?= $this->header('Správa uživatelů') ?>
<div class="container">
  <h2>Nepotvrzení uživatelé</h2>
  <form action="" method="POST">
    <?= $this->table()
           ->style('width:100%')
           ->column('buttons', '')
           ->column('group', 'Úroveň')
           ->column('skupina', 'Skupina')
           ->column('dancer', 'Aktivní<br/>tanečník', 'center')
           ->column('fullName', 'Jméno', '', 'max-width:140px')
           ->column('narozeni', 'Datum<br/>narození')
           ->column('poznamky', 'Poznámka')
           ->data($this->data) ?>
  </form>
</div>
