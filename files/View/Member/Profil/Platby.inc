<?= $this->header('Moje platby') ?>

<div class="container full">
  <?php echo $this->colorBox, '&nbsp;', $this->skupinaData;?><br/>
  Variabilní symbol: <?php echo $this->varSymbol;?>
  <br/><br/>
  Aktuálně: <?php echo $this->zaplacenoText;?>
  <br/>

  <?php if (!empty($this->platby)): ?>
    <h2>Historie plateb</h2>
    <table>
      <tr>
        <td>Skupina</td>
        <td>Období</td>
        <td>Částka</td>
        <td>Placeno</td>
        <td>Platí do</td>
      </tr>
      <?php foreach ($this->platby as $platba): ?>
        <tr>
          <td><?php echo $platba['colorBox'];?></td>
          <td><?php echo $platba['obdobi'];?> Kč</td>
          <td><?php echo $platba['castka'];?> Kč</td>
          <td><?php echo $platba['datum'];?></td>
          <td><?php echo $platba['platnost'];?></td>
        </tr>
      <?php endforeach;?>
    </table><br/>
  <?php endif; ?>

  <h2>Přehled plateb</h2>
  <?php
  echo $this->table()
            ->data($this->platbyGroups)
            ->column('name', 'Jméno', '', 'width:125px')
            ->column('type', 'Typ platby')
            ->column('symbol', 'Specifický symbol', '', 'width:75px')
            ->column('amount', 'Částka')
            ->column('dueDate', 'Splatnost')
            ->column('validRange', 'Platnost');
  echo '<a href="/member/profil">Zpět</a>';
  ?>
</div>
