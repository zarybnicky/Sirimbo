<div class="container">
  <form action="" method="POST">
    <div class="form-group">
      <label for="jmeno">Název akce</label>
      <?= (new \TextHelper('jmeno', $jmeno))->placeholder('Název akce') ?>
    </div>
    <div class="form-group">
      <label for="kde">Název akce</label>
      <?= (new \TextHelper('kde', $kde))->placeholder('Místo akce') ?>
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
      <?= new \TextHelper('kapacita', $kapacita) ?>
    </div>
    <div class="form-check">
      <?= new \CheckboxHelper('visible', 'visible', $visible, 'Zviditelnit') ?>
    </div>
    <div class="form-check">
      <?= new \CheckboxHelper('lock', 'lock', $lock, 'Uzamknout') ?>
    </div>
    <div class="form-group">
      <?= new \SubmitHelper($action) ?>
      <a class="btn btn-outline-primary" href="/admin/akce">Zpět</a>
    </div>
  </form>
</div>
