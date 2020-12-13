<div class="container">
  <?= (new PartnerRequestHelper())->getAll() ?>
  <div class="mb-2">
    <a class="btn btn-outline-primary" href="/member/profil/edit">Upravit osobní údaje</a>
    <a class="btn btn-outline-primary" href="/member/profil/heslo">Změnit heslo</a>
    <a class="btn btn-outline-primary" href="/member/profil/par/partner">Změnit partnera</a>
    <?php if (isset($this->coupleData) && $this->coupleData['u_id']) : ?>
      <a class="btn btn-outline-primary" href="/member/profil/par/body">Změnit třídu a body</a>
    <?php endif ?>
    <a class="btn btn-outline-primary" href="/logout">Odhlásit se</a>
  </div>

  <div class="mb-2">
    Tréninková skupina: <?= $this->skupina ?><br>
    Věková kategorie: <?= $this->ageGroup ?>
  </div>

  <?php if (isset($this->coupleData) && $this->coupleData['u_id']) : ?>
    <h3>Pár</h3>
    <?php $p = $this->coupleData ?>
    <div class="mb-2">
      Partner(ka): <?= $p['u_jmeno'] ?> <?= $p['u_prijmeni'] ?><br>
      Standardní tance:
      <?= $p['p_stt_trida'] ?> <?= $p['p_stt_body'] ?>, <?= $p['p_stt_finale'] ?>F
      <br>
      Latinsko-americké tance:
      <?= $p['p_lat_trida'] ?> <?= $p['p_lat_body'] ?>, <?= $p['p_lat_finale'] ?>F
    </div>
  <?php endif ?>

  <h3>Platby</h3>
  <div class="mb-2">
    Variabilní symbol: <?= $this->varSymbol ?><br>
    Členské příspěvky: <?= $this->hasPaid ? 'zaplaceno' : 'nezaplaceno' ?><br>
    Č.ú.: 1806875329/0800
  </div>

  <?php if (!empty($this->paymentsWanted)) : ?>
    <h3 class="mt-2">K zaplacení</h3>
    <?= (new TableHelper($this->paymentsWanted))
           ->column('name', 'Jméno')
           ->column('type', 'Typ platby')
           ->column('symbol', 'Spec. symbol', '', 'width:75px')
           ->column('amount', 'Částka')
           ->column('dueDate', 'Splatnost')
           ->column('validRange', 'Platnost') ?>
  <?php endif ?>

  <?php if (!empty($this->paymentHistory)) : ?>
    <h3 class="mt-2">Historie plateb</h3>
    <?= (new TableHelper($this->paymentHistory))
           ->column('type', 'Typ')
           ->column('varSymbol', 'Spec. symbol')
           ->column('amount', 'Částka')
           ->column('paidOn', 'Placeno')
           ->column('validFor', 'Platí do') ?>
  <?php endif ?>

</div>
