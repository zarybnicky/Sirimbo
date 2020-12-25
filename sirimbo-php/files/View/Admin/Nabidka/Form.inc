<div class="container">
  <form action="" method="POST">
    <button type="submit" style="display:none;">Enter</button>
    <div class="form-group">
      <label for="trewner">Trenér</label>
      <?= \Utils::userSelect($users, 'trener', $trener) ?>
    </div>
    <div class="form-group">
      <label for="pocet_hod">Počet hodin</label>
      <?= \Utils::text('pocet_hod', $pocet_hod) ?>
    </div>
    <div class="form-group">
      <label for="max_pocet_hod">Max. počet hodin/pár</label>
      <?= \Utils::text('max_pocet_hod', $max_pocet_hod) ?>
      <small class="form-text text-muted">"0" = neomezeně</small>
    </div>
    <div class="form-group">
      <label for="od">Od</label>
      <?= new \DateHelper('od', $od) ?>
    </div>
    <div class="form-group">
      <label for="do">Do</label>
      <?= new \DateHelper('do', $do) ?>
      <small class="form-text text-muted">(pokud je prázdné, počítá se jako "Od")</small>
    </div>
    <div class="form-check">
      <?= \Utils::checkbox('visible', 'visible', $visible, 'Zviditelnit') ?>
    </div>
    <div class="form-check">
      <?= \Utils::checkbox('lock', 'lock', $lock, 'Uzamknout') ?>
    </div>
    <div class="form-group">
      <input type='hidden' name='returnURI' value='<?= $returnURI ?>'>
      <?= \Utils::submit($action) ?>
      <a class="btn btn-outline-primary" href="<?= $returnURI ?>">Zpět</a>
    </div>
  </form>
</div>
