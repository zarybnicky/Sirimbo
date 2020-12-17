<div class="container">
  <?php
  if (!count($this->files)) {
      echo new NoticeHelper('Žádné fotky');
      return;
  }
  ?>
  <form action="" method="POST">
    <a class="btn btn-primary" href="/admin/galerie/file/upload?dir=<?= $this->id ?>">Přidat fotky</a><br>
    <?php foreach ($this->files as $item) : ?>
      <div class="foto_preview">
        <div class="foto_preview_in">
          <img alt="<?= $item['id'] ?>" src="<?= $item['thumbnailURI']?>" />
        </div>
        <div class="foto_desc">
          <?= \Buttons::galleryFoto($item['id']) ?>
          <?= $item['name'] ?>
        </div>
      </div>
    <?php endforeach ?>
  </form>
</div>
