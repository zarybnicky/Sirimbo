<?= $this->header('Správa videí') ?>

<div class="container">
  <form action="" method="post">
    Titulní stránka:<br>
    <?= $this->video1 ?> <?= $this->video2 ?><br>
    <?= $this->video3 ?> <?= $this->video4 ?><br>
    <?= $this->submit('Uložit') ?><br>
    <a class="btn btn-primary" href="/admin/video/add">Nové video</a>
  </form>
  <form action="" method="post">
    <?php if ($this->data): ?>
      <?= $this->table()
             ->style('width:100%')
             ->data($this->data)
             ->column('buttons', '', null, 'width:55px')
             ->column('name', 'Jméno')
             ->column('uri', 'URI', null)
             ->column('playlist', 'V playlistu?')
             ->column('date', 'Datum přidání', null, 'width:100px') ?>
    <?php else: ?>
      <?= $this->notice('Žádná videa') ?>
    <?php endif ?>
  </form>
</div>
