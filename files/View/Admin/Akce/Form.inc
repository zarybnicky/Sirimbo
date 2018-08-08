<?= $this->header('Správa akcí', $this->header) ?>
<div class="container">
  <form action="" method="POST">
    <div class="form-group">
      <label for="jmeno">Název akce</label>
      <?= $this->text('jmeno', $this->jmeno)->placeholder('Název akce') ?>
    </div>
    <div class="form-group">
      <label for="kde">Název akce</label>
      <?= $this->text('kde', $this->kde)->placeholder('Místo akce') ?>
    </div>
    <div class="form-group">
      <label for="info">Další info</label>
      <textarea class="form-control" name="info" rows="15"><?= $this->info ?></textarea>
    </div>
    <div class="form-group">
      <label for="od">Den začátku (*)</label>
      <?= $this->date('od', $this->od) ?>
    </div>
    <div class="form-group">
      <label for="od">Den konce (*)</label>
      <?= $this->date('do', $this->do) ?>
      <small class="form-text text-muted">(pokud je prázdné, počítá se jako "Od")</small>
    </div>
    <div class="form-group">
      <label for="kapacita">Kapacita (*)</label>
      <?= $this->text('kapacita', $this->kapacita) ?>
    </div>
    <div class="form-check">
      <?= $this->checkbox('visible', 'visible')->label('Zviditelnit')->set($this->visible) ?>
    </div>
    <div class="form-check">
      <?= $this->checkbox('lock')->label('Uzamknout')->set($this->lock) ?>
    </div>
    <div class="form-group">
      <?= $this->submit($this->action) ?>
      <a class="btn btn-outline-primary" href="/admin/akce">Zpět</a>
    </div>
  </form>
</div>
