<div class="container">
  <?= $this->partnerRequest()->getAll() ?>
  <?php if ($this->havePartner): ?>
    Právě tančím s: <?= $this->partnerFullName ?>
    <h2>Třídy a body:</h2>
    <h3>Standardní tance:</h3>
    <br />
    Třída: <span style="font-weight:bolder;"><?= $this->sttTrida ?></span>,
    body: <?= $this->sttBody ?>,
    finále: <?= $this->sttFinale ?>
    <br />
    <h3>Latinsko-americké tance:</h3>
    <br />
    Třída: <span style="font-weight:bolder;"><?= $this->latTrida ?></span>,
    body: <?= $this->latBody ?>,
    finále: <?= $this->latFinale ?>
    <br /><br/>
    V "Olympáckém žebříčku" máme <?= $this->hodnoceni ?> bodů.
    <br />
    <a href="/member/profil/par/body">Změnit třídu a body</a>
  <?php else: ?>
    Právě s nikým netančím.<br/>
  <?php endif ?>
  <a class="btn btn-outline-primary" href="/member/profil/par/partner">Změnit partnera</a>
  <a class="btn btn-outline-primary" href="/member/profil">Zpět</a>
</div>
