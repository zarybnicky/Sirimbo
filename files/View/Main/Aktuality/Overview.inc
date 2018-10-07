<div class="container">
  <?php foreach ($this->data as $row) : ?>
    <div class="row mb-2">
      <div class="col-md-auto align-self-center">
        <img style="max-width:100%" src="<?= $row['title_photo_thumb_uri'] ?>" />
      </div>
      <div class="col align-self-center">
        <h3><a href="/aktualne/<?= $row['id'] ?>"><?= $row['jmeno'] ?></a></h3>
        <small>
          <?= formatTimestamp($row['timestamp'])?>
          <?php if ($row['canEdit']) : ?>
            - <a href="/admin/aktuality/edit/<?= $row['id'] ?>">Upravit</a>
          <?php endif ?>
        </small><br>
        <p><?= $row['preview'] ?></p>
      </div>
    </div>
  <?php endforeach ?>
</div>
