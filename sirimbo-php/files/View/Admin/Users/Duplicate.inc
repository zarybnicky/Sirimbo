<div class="container">
  <form action="" method="POST">
    <?= $this->table()
           ->style('width:100%')
           ->column('buttons', '')
           ->column('fullName', 'Jméno')
           ->column('email', 'Email')
           ->column('telefon', 'Telefon')
           ->column('narozeni', 'Datum<br/>narození')
           ->column('timestamp', 'Poslední<br/>úprava')
           ->data($this->data) ?>
  </form>
</div>
