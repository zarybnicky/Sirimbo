<?= $this->header('Správa párů', 'Změna třídy a bodů') ?>

<div class="container">
  <form action="" method="POST">
    <h3>Pár: <?= $this->fullName ?></h3>
    <h4>Standard</h4>
    <div class="form-group">
      <label for="stt-trida">Třída</label>
      <?= $this->select('stt-trida')
             ->set($this->stt_trida)
             ->options(['Z', 'H', 'D', 'C', 'B', 'A', 'M'], true, true) ?>
    </div>
    <div class="form-group">
      <label for="stt-body">Body</label>
      <?= $this->text('stt-body', $this->stt_body) ?>
    </div>
    <div class="form-group">
      <label for="stt-finale">Finále</label>
      <?= $this->text('stt-finale', $this->stt_finale) ?>
    </div>

    <h4>Latina</h4>
    <div class="form-group">
      <label for="lat-trida">Třída</label>
      <?= $this->select('lat-trida')
             ->set($this->lat_trida)
             ->options(['Z', 'H', 'D', 'C', 'B', 'A', 'M'], true, true) ?>
    </div>
    <div class="form-group">
      <label for="lat-body">Body</label>
      <?= $this->text('lat-body', $this->lat_body) ?>
    </div>
    <div class="form-group">
      <label for="lat-finale">Finále</label>
      <?= $this->text('lat-finale', $this->lat_finale) ?>
    </div>
    <div class="form-group">
      <?= $this->submit('Upravit')->data('action', 'edit_confirm') ?>
      <a class="btn btn-outline-primary" href="/admin/pary">Zpět</a>
    </div>
</div>
