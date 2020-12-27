<div class="container">
  <form action="" method="POST">
    <h3>Pár: <?= $fullName ?></h3>
    <h4>Standard</h4>
    <div class="form-group">
      <label for="stt-trida">Třída</label>
      <?= \Utils::selectLiteral('stt-trida', ['Z', 'H', 'D', 'C', 'B', 'A', 'M'], $stt_trida) ?>
    </div>
    <div class="form-group">
      <label for="stt-body">Body</label>
      <?= \Utils::text('stt-body', $stt_body) ?>
    </div>
    <div class="form-group">
      <label for="stt-finale">Finále</label>
      <?= \Utils::text('stt-finale', $stt_finale) ?>
    </div>

    <h4>Latina</h4>
    <div class="form-group">
      <label for="lat-trida">Třída</label>
      <?= \Utils::selectLiteral('lat-trida', ['Z', 'H', 'D', 'C', 'B', 'A', 'M'], $lat_trida) ?>
    </div>
    <div class="form-group">
      <label for="lat-body">Body</label>
      <?= \Utils::text('lat-body', $lat_body) ?>
    </div>
    <div class="form-group">
      <label for="lat-finale">Finále</label>
      <?= \Utils::text('lat-finale', $lat_finale) ?>
    </div>
    <div class="form-group">
      <button name="action" value="edit_confirm" class="btn btn-primary">Upravit</button>
      <a class="btn btn-outline-primary" href="/admin/pary">Zpět</a>
    </div>
</div>
