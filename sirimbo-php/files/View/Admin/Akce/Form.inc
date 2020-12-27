<div class="container">
  <form action="" method="POST">
    <div class="form-group">
      <label for="jmeno">Název akce</label>
      <?= \Utils::text('jmeno', $jmeno, ['placeholder' => 'Název akce']) ?>
    </div>
    <div class="form-group">
      <label for="kde">Název akce</label>
      <?= \Utils::text('kde', $kde, ['placeholder' => 'Místo akce']) ?>
    </div>
    <div class="form-group">
      <label for="info">Další info</label>
      <textarea class="form-control" name="info" rows="15"><?= $info ?></textarea>
    </div>
    <div class="form-group">
      <label for="od">Den začátku (*)</label>
      <?= new \DateHelper('od', $od) ?>
    </div>
    <div class="form-group">
      <label for="od">Den konce (*)</label>
      <?= new \DateHelper('do', $do) ?>
      <small class="form-text text-muted">(pokud je prázdné, počítá se jako "Od")</small>
    </div>
    <div class="form-group">
      <label for="kapacita">Kapacita (*)</label>
      <?= \Utils::text('kapacita', $kapacita) ?>
    </div>
    <div class="form-check">
      <?= \Utils::checkbox('visible', 'visible', $visible, 'Zviditelnit') ?>
    </div>
    <div class="form-check">
      <?= \Utils::checkbox('lock', 'lock', $lock, 'Uzamknout') ?>
    </div>
    <div class="form-group">
      <button class="btn btn-primary"><?= $action ?></button>
      <a class="btn btn-outline-primary" href="/admin/akce">Zpět</a>
    </div>
  </form>
</div>
