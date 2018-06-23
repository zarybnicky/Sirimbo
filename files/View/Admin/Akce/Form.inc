<?= $this->header('Správa akcí', $this->header) ?>
<div class="container">
  <form action="" method="POST">
    <div class="form-group">
      <label for="jmeno">Název akce</label>
      <input type="text" name="jmeno" placeholder="Název akce"
             class="form-control" aria-describedby="jmenoHelp"
             value="<?= $this->jmeno ?>" />
      <small id="jmenoHelp" class="form-text text-muted">Nejvíce 255 znaků</small>
    </div>
    <div class="form-group">
      <label for="kde">Název akce</label>
      <input type="text" name="kde" placeholder="Místo akce"
             class="form-control" aria-describedby="kdeHelp"
             value="<?= $this->kde ?>" />
      <small id="kdeHelp" class="form-text text-muted">Nejvíce 255 znaků</small>
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
      <small class="form-text text-muted">(pokud je prázdný, počítá se jako "Od")</small>
    </div>
    <div class="form-group">
      <label for="kapacita">Kapacita (*)</label>
      <?= $this->text('kapacita', $this->kapacita) ?>
      <small class="form-text text-muted">Pouze číslice</small>
    </div>
    <div class="form-check">
      <?= $this->checkbox('visible', 'visible')->label('Uzamknout')->set($this->visible) ?>
    </div>
    <div class="form-check">
      <?= $this->checkbox('visible', 'visible')->label('Zviditelnit')->set($this->visible) ?>
    </div>
    <div class="form-group">
      <?= $this->submit($this->action) ?>
      <a class="btn btn-outline-primary" href="/admin/akce">Zpět</a>
    </div>
  </form>
</div>
