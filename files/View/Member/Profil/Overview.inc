<?= $this->header('Profil') ?>

<div class="container full">
  <?php echo $this->notice($this->redirect()->getMessage());?>

  <a href="/member/profil/edit">Upravit osobní údaje</a><br/>
  <a href="/member/profil/heslo">Změnit heslo</a><br/>
  <a href="/member/profil/par">Detaily páru</a><br/>
  <a href="/member/profil/platby">Platby</a><br/>
</div>
