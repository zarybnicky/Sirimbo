<div class="container">
  <?= $this->colorBox, '&nbsp;', $this->skupinaData ?><br>
  Č.ú.: 1806875329/0800<br>
  Variabilní symbol: <?= $this->varSymbol ?><br>
  <br/>
  <?= $this->zaplacenoText ? "Aktuálně: {$this->zaplacenoText}<br><br>" : '' ?>

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
          <td><?= $platba['colorBox'] ?></td>
          <td><?= $platba['obdobi'] ?> Kč</td>
          <td><?= $platba['castka'] ?> Kč</td>
          <td><?= $platba['datum'] ?></td>
          <td><?= $platba['platnost'] ?></td>
        </tr>
      <?php endforeach ?>
    </table><br/>
  <?php endif ?>

  <h2>Výběr plateb</h2>
  <?= $this->table()
           ->data($this->platbyGroups)
           ->column('name', 'Jméno', '', 'width:125px')
           ->column('type', 'Typ platby')
           ->column('symbol', 'Specifický symbol', '', 'width:75px')
           ->column('amount', 'Částka')
           ->column('dueDate', 'Splatnost')
           ->column('validRange', 'Platnost') ?>
  <a class="btn btn-outline-primary" href="/member/profil">Zpět</a>
</div>
