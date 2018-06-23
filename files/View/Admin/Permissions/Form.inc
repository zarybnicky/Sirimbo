<?= $this->header(
    'Správa oprávnění',
    ($this->action == 'add')
        ? 'Přidat uživatelskou skupinu'
        : 'Upravit uživatelskou skupinu'
) ?>
<div class="container full">
  <form action="" method="POST">
    <div class="form-group row">
      <label for="name" class="col-sm-2 col-form-label">Jméno</label>
      <div class="col-sm-10">
        <?= $this->text('name', $this->name)->cls('form-control') ?>
      </div>
    </div>
    <div class="form-group row">
      <label for="description" class="col-sm-2 col-form-label">Popis</label>
      <div class="col-sm-10">
        <?= $this->text('description', $this->description)->size(35)->cls('form-control') ?>
      </div>
    </div>
    <?php foreach ($this->settings as $item): ?>
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
        <?= $this->submit($this->action == 'add' ? 'Přidat' : 'Upravit') ?>
        <a class="btn btn-outline-primary" href="/admin/permissions">Zpět</a>
      </div>
    </div>
  </form>
</div>
