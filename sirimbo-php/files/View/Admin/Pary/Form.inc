<div class="container">
  <form action="" method="POST">
    <h3>Pár: <?= $this->fullName ?></h3>
    <h4>Standard</h4>
    <div class="form-group">
      <label for="stt-trida">Třída</label>
      <?= (new \SelectHelper('stt-trida', [], $this->stt_trida))
             ->options(['Z', 'H', 'D', 'C', 'B', 'A', 'M'], true, true) ?>
    </div>
    <div class="form-group">
      <label for="stt-body">Body</label>
      <?= new \TextHelper('stt-body', $this->stt_body) ?>
    </div>
    <div class="form-group">
      <label for="stt-finale">Finále</label>
      <?= new \TextHelper('stt-finale', $this->stt_finale) ?>
    </div>

    <h4>Latina</h4>
    <div class="form-group">
      <label for="lat-trida">Třída</label>
      <?= (new \SelectHelper('lat-trida', [], $this->lat_trida))
             ->options(['Z', 'H', 'D', 'C', 'B', 'A', 'M'], true, true) ?>
    </div>
    <div class="form-group">
      <label for="lat-body">Body</label>
      <?= new \TextHelper('lat-body', $this->lat_body) ?>
    </div>
    <div class="form-group">
      <label for="lat-finale">Finále</label>
      <?= new \TextHelper('lat-finale', $this->lat_finale) ?>
    </div>
    <div class="form-group">
      <?= (new \SubmitHelper('Upravit'))->data('action', 'edit_confirm') ?>
      <a class="btn btn-outline-primary" href="/admin/pary">Zpět</a>
    </div>
</div>
