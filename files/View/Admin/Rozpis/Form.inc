<div class="container">
  <form action="" method="POST">
    <div class="form-group">
      <label for="trener">Trenér</label>
      <?= $this->userSelect('trener')
             ->set($this->trener)
             ->tmpSwitch($this->isAdmin)
             ->users($this->treneri) ?>
    </div>
    <div class="form-group">
      <label for="kde">Místo</label>
      <?= $this->text('kde', $this->kde) ?>
    </div>
    <div class="form-group">
      <label for="datum">Datum</label>
      <?= $this->date('datum', $this->datum) ?>
    </div>
    <div class="form-check">
      <?= $this->checkbox('visible', 'visible')->label('Zviditelnit')->set($this->visible) ?>
    </div>
    <div class="form-check">
      <?= $this->checkbox('lock')->label('Uzamknout')->set($this->lock) ?>
    </div>
    <div class="form-group">
      <?= $this->submit($this->action == 'add' ? 'Přidat' : 'Upravit') ?>
      <a class="btn btn-outline-primary" href="/admin/rozpis">Zpět</a>
    </div>
  </form>
</div>
