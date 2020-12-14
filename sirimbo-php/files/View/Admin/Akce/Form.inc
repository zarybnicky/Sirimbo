<div class="container">
  <form action="" method="POST">
    <div class="form-group">
      <label for="jmeno">Název akce</label>
      <?= (new \TextHelper('jmeno', $this->jmeno))->placeholder('Název akce') ?>
    </div>
    <div class="form-group">
      <label for="kde">Název akce</label>
      <?= (new \TextHelper('kde', $this->kde))->placeholder('Místo akce') ?>
    </div>
    <div class="form-group">
      <label for="info">Další info</label>
      <textarea class="form-control" name="info" rows="15"><?= $this->info ?></textarea>
    </div>
    <div class="form-group">
      <label for="od">Den začátku (*)</label>
      <?= new \DateHelper('od', $this->od) ?>
    </div>
    <div class="form-group">
      <label for="od">Den konce (*)</label>
      <?= new \DateHelper('do', $this->do) ?>
      <small class="form-text text-muted">(pokud je prázdné, počítá se jako "Od")</small>
    </div>
    <div class="form-group">
      <label for="kapacita">Kapacita (*)</label>
      <?= new \TextHelper('kapacita', $this->kapacita) ?>
    </div>
    <div class="form-check">
      <?= new \CheckboxHelper('visible', 'visible', $this->visible, 'Zviditelnit') ?>
    </div>
    <div class="form-check">
      <?= new \CheckboxHelper('lock', 'lock', $this->lock, 'Uzamknout') ?>
    </div>
    <div class="form-group">
      <?= new \SubmitHelper($this->action) ?>
      <a class="btn btn-outline-primary" href="/admin/akce">Zpět</a>
    </div>
  </form>
</div>
