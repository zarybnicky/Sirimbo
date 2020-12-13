<div class="container">
  <form action="" method="POST">
    <?= (new TableHelper($this->data))
           ->style('width:100%')
           ->column('buttons', '')
           ->column('fullNameMan', 'Partner')
           ->column('fullNameWoman', 'Partnerka')
           ->column('standard', 'Standard')
           ->column('latina', 'Latina')
           ->column('hodnoceni', 'Body') ?>
    <br/>
    <h2>Nový pár</h2>
    Partner: <?= new UserSelectHelper($this->usersMen, 'add_partner') ?><br/>
    Partnerka: <?= new UserSelectHelper($this->usersWomen, 'add_partnerka') ?><br/>
    <?= $this->submit('Nový pár')->data('action', 'add') ?>
    <button class="btn btn-outline-primary" name="action" value="fix_unpaired">Opravit nespárované uživatele</button>
  </form>
</div>
