<div class="container">
  <form action="" method="POST">
    <h3>Pár: <?= $fullName ?></h3>
    <h4>Standard</h4>
    <div class="form-group">
      <label for="stt-trida">Třída</label>
      <?= (new \SelectHelper('stt-trida', [], $stt_trida))
             ->options(['Z', 'H', 'D', 'C', 'B', 'A', 'M'], true, true) ?>
    </div>
    <div class="form-group">
      <label for="stt-body">Body</label>
      <?= new \TextHelper('stt-body', $stt_body) ?>
    </div>
    <div class="form-group">
      <label for="stt-finale">Finále</label>
      <?= new \TextHelper('stt-finale', $stt_finale) ?>
    </div>

    <h4>Latina</h4>
    <div class="form-group">
      <label for="lat-trida">Třída</label>
      <?= (new \SelectHelper('lat-trida', [], $lat_trida))
             ->options(['Z', 'H', 'D', 'C', 'B', 'A', 'M'], true, true) ?>
    </div>
    <div class="form-group">
      <label for="lat-body">Body</label>
      <?= new \TextHelper('lat-body', $lat_body) ?>
    </div>
    <div class="form-group">
      <label for="lat-finale">Finále</label>
      <?= new \TextHelper('lat-finale', $lat_finale) ?>
    </div>
    <div class="form-group">
      <?= (new \SubmitHelper('Upravit'))->data('action', 'edit_confirm') ?>
      <a class="btn btn-outline-primary" href="/admin/pary">Zpět</a>
    </div>
</div>
