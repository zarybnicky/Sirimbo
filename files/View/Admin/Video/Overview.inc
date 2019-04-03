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
             ->column('title', 'Jméno')
             ->column('uri', 'ID videa', null)
             ->column('playlist', 'Playlist')
             ->column('created', 'Přidáno', null, 'width:100px') ?>
    <?php else: ?>
      <?= $this->notice('Žádná videa') ?>
    <?php endif ?>
  </form>
</div>
