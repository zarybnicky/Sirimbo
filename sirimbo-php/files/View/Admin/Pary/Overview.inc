<div class="container">
  <form action="" method="POST">
    <?= $this->table()
           ->style('width:100%')
           ->data($this->data)
           ->column('buttons', '')
           ->column('fullNameMan', 'Partner')
           ->column('fullNameWoman', 'Partnerka')
           ->column('standard', 'Standard')
           ->column('latina', 'Latina')
           ->column('hodnoceni', 'Body') ?>
    <br/>
    <h2>Nový pár</h2>
    Partner: <?= $this->userSelect('add_partner')->users($this->usersMen) ?><br/>
    Partnerka: <?= $this->userSelect('add_partnerka')->users($this->usersWomen) ?><br/>
    <?= $this->submit('Nový pár')->data('action', 'add') ?>
    <button class="btn btn-outline-primary" name="action" value="fix_unpaired">Opravit nespárované uživatele</button>
  </form>
</div>
