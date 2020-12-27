<div class="container full">
  <form action="" method="POST">
    <div class="form-group row">
      <label for="name" class="col-sm-2 col-form-label">Jméno</label>
      <div class="col-sm-10">
        <?= \Utils::text('name', $name) ?>
      </div>
    </div>
    <div class="form-group row">
      <label for="description" class="col-sm-2 col-form-label">Popis</label>
      <div class="col-sm-10">
        <?= \Utils::text('description', $description, ['size' => 35]) ?>
      </div>
    </div>
    <?php foreach ($settings as $item): ?>
      <div class="form-group row">
        <div class="col-sm-2"><?= $item['name'] ?></div>
        <?php foreach ($item['items'] as $level): ?>
          <div class="col-sm-2"><?= $level ?></div>
        <?php endforeach ?>
      </div>
    <?php endforeach ?>
    <div class="form-group row">
      <div class="col-sm-2"></div>
      <div class="col-sm-10">
        <button class="btn btn-primary"><?= $action ?></button>
        <a class="btn btn-outline-primary" href="/admin/permissions">Zpět</a>
      </div>
    </div>
  </form>
</div>
