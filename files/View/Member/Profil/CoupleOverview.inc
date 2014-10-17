<?= $this->header('Profil') ?>

<div class="container">
  <?php
  echo $this->partnerRequest()->getAll();
  echo $this->notice($this->redirect()->getMessage());
  ?>
  <?php if ($this->havePartner): ?>
    Právě tančím s: <?php echo $this->partnerFullName;?>
    <h2>Třídy a body:</h2>
    <h3>Standardní tance:</h3>
    <br />
    Třída: <span style="font-weight:bolder;"><?php echo $this->sttTrida;?></span>,
    body: <?php echo $this->sttBody;?>,
    finále: <?php echo $this->sttFinale;?>
    <br />
    <h3>Latinsko-americké tance:</h3>
    <br />
    Třída: <span style="font-weight:bolder;"><?php echo $this->latTrida;?></span>,
    body: <?php echo $this->latBody;?>,
    finále: <?php echo $this->latFinale;?>
    <br /><br/>
    V "Olympáckém žebříčku" máme <?php echo $this->hodnoceni;?> bodů.
    <br />
    <a href="/member/profil/par/body">Změnit třídu a body</a>
  <?php else: ?>
    Právě s nikým netančím.<br/>
  <?php endif;?>
  <a href="/member/profil/par/partner">Změnit partnera</a>
  <a href="/member/profil">Zpět</a>
</div>
