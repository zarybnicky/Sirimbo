<div class="container">
  <?php
  echo \Utils::partnerRequest();
  if ($havePartner) {
      echo "Právě tančím s: ", $partnerFullName;
  } else {
      echo 'Právě s nikým netančím';
  }
  ?>
  <form method="POST" action="">
    <?= \Utils::userSelect($users, 'partner') ?>
    <button name="action" value="confirm" class="btn btn-primary">Požádat o partnerství</button>
    <?php if ($havePartner) : ?>
      <button name="action" value="dumpthen" class="btn btn-primary">Rozejít se</button>
    <?php endif ?>
    <a class="btn btn-outline-primary" href="/member/profil">Zpět</a>
  </form>
</div>
