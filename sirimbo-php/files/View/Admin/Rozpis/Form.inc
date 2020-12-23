<div class="container">
  <form action="" method="POST">
    <div class="form-group">
      <label for="trener">Trenér</label>
      <?= (new UserSelectHelper($treneri, 'trener'))->set($trener) ?>
    </div>
    <div class="form-group">
      <label for="kde">Místo</label>
      <?= new \TextHelper('kde', $kde) ?>
    </div>
    <div class="form-group">
      <label for="datum">Datum</label>
      <?= new \DateHelper('datum', $datum) ?>
    </div>
    <div class="form-check">
      <?= new \CheckboxHelper('visible', 'visible', $visible, 'Zviditelnit') ?>
    </div>
    <div class="form-check">
      <?= new \CheckboxHelper('lock', 'lock', $lock, 'Uzamknout') ?>
    </div>
    <div class="form-group">
      <?= new \SubmitHelper($action == 'add' ? 'Přidat' : 'Upravit') ?>
      <a class="btn btn-outline-primary" href="/admin/rozpis">Zpět</a>
    </div>
  </form>
</div>
