<div class="container">
  <form action="" method="POST">
    <div class="form-group">
      <label for="trener">Trenér</label>
      <?= (new UserSelectHelper($this->treneri, 'trener'))->set($this->trener) ?>
    </div>
    <div class="form-group">
      <label for="kde">Místo</label>
      <?= new \TextHelper('kde', $this->kde) ?>
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
      <?= new \SubmitHelper($this->action == 'add' ? 'Přidat' : 'Upravit') ?>
      <a class="btn btn-outline-primary" href="/admin/rozpis">Zpět</a>
    </div>
  </form>
</div>
