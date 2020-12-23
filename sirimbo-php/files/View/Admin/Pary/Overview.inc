<div class="container">
  <form action="" method="POST">
    <?= (new \TableHelper($data))
           ->style('width:100%')
           ->column('buttons', '')
           ->column('fullNameMan', 'Partner')
           ->column('fullNameWoman', 'Partnerka')
           ->column('standard', 'Standard')
           ->column('latina', 'Latina')
           ->column('hodnoceni', 'Body') ?>
    <br/>
    <h2>Nový pár</h2>
    Partner: <?= \Utils::userSelect($usersMen, 'add_partner') ?><br/>
    Partnerka: <?= \Utils::userSelect($usersWomen, 'add_partnerka') ?><br/>
    <?= \Utils::submit('Nový pár', 'action', 'add') ?>
    <button class="btn btn-outline-primary" name="action" value="fix_unpaired">Opravit nespárované uživatele</button>
  </form>
</div>
