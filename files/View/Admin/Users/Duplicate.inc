<?= $this->header('Správa uživatelů') ?>
<div class="container">
  <h2>Duplicitní uživatelé</h2>
  <form action="" method="POST">
    <?= $this->menu()
           ->content('Odstranit', 'remove', true)
           ->content('Zpět', '../users') ?>
    <?= $this->table()
           ->column('checkBox', '')
           ->column('colorBox', '')
           ->column('fullName', 'Jméno', '', 'max-width:140px;')
           ->column('email', 'Email')
           ->column('telefon', 'Telefon')
           ->column('narozeni', 'Datum<br/>narození')
           ->column('timestamp', 'Poslední<br/>úprava')
           ->data($this->data) ?>
  </form>
</div>
