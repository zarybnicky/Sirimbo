<?= $this->header('Správa nabídky', $this->header) ?>
<div class="container">
  <form action="" method="POST">
    <button type="submit" style="display:none;">Enter</button>
    <div class="form-group">
      <label for="trewner">Trenér</label>
      <?= $this->userSelect('trener')
             ->users($this->users)
             ->set($this->trener)
             ->tmpSwitch($this->isAdmin) ?>
    </div>
    <div class="form-group">
      <label for="pocet_hod">Počet hodin</label>
      <?= $this->text('pocet_hod', $this->pocet_hod) ?>
    </div>
    <div class="form-group">
      <label for="max_pocet_hod">Max. počet hodin/pár</label>
      <?= $this->text('max_pocet_hod', $this->max_pocet_hod) ?>
      <small class="form-text text-muted">"0" = neomezeně</small>
    </div>
    <div class="form-group">
      <label for="od">Od</label>
      <?= $this->date('od', $this->od) ?>
    </div>
    <div class="form-group">
      <label for="do">Do</label>
      <?= $this->date('od', $this->od) ?>
      <small class="form-text text-muted">(pokud je prázdné, počítá se jako "Od")</small>
    </div>
    <div class="form-check">
      <?= $this->checkbox('visible', 'visible')->label('Zviditelnit')->set($this->visible) ?>
    </div>
    <div class="form-check">
      <?= $this->checkbox('lock')->label('Uzamknout')->set($this->lock) ?>
    </div>
    <div class="form-group">
      <?= $this->hidden('referer', $this->referer) ?>
      <?= $this->submit($this->action) ?>
      <a class="btn btn-outline-primary" href="<?= $this->referer ?>">Zpět</a>
    </div>
  </form>
</div>
