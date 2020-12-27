<div class="container">
  <form action="" method="POST">
    <div class="form-group">
      <label for="trener">Trenér</label>
      <?= \Utils::userSelect($treneri, 'trener', $trener) ?>
    </div>
    <div class="form-group">
      <label for="kde">Místo</label>
      <?= \Utils::text('kde', $kde) ?>
    </div>
    <div class="form-group">
      <label for="datum">Datum</label>
      <?= new \DateHelper('datum', $datum) ?>
    </div>
    <div class="form-check">
      <?= \Utils::checkbox('visible', 'visible', $visible, 'Zviditelnit') ?>
    </div>
    <div class="form-check">
      <?= \Utils::checkbox('lock', 'lock', $lock, 'Uzamknout') ?>
    </div>
    <div class="form-group">
      <button class="btn btn-primary"><?= $action ?></button>
      <a class="btn btn-outline-primary" href="/admin/rozpis">Zpět</a>
    </div>
  </form>
</div>
