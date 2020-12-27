<div class="container">
  <form action="" method="POST">
    <div class="form-group">
      <label for="nadpis">Nadpis</label>
      <?= \Utils::text('nadpis', $nadpis) ?>
    </div>
    <div class="form-group">
      <label for="text">Text</label>
      <textarea name="text" id="text" rows="30" cols="60" style="width:99%">
        <?= $text ?>
      </textarea>
      <script src="/scripts/ckeditor/ckeditor.js"></script>
      <script>
       CKEDITOR.config.allowedContent = true;
       CKEDITOR.replace('text');
      </script>
    </div>
    <div class="form-group">
      <label>Skupiny</label>
      <?php foreach ($skupiny as $item) : ?>
        <div class="form-check">
          <?= \Utils::checkbox(
              'sk-' . $item['s_id'],
              '1',
              $skupinySelected[$item['s_id']],
              "<div class=\"box\" title=\"{$item['s_description']}\" style=\"background-color:{$item['s_color_rgb']}\"></div>"
              . '&nbsp;' . $item['s_name']
          ) ?>
        </div>
      <?php endforeach ?>
    </div>
    <br>
    <div class="form-group">
      <div class="form-check">
        <?= \Utils::checkbox('lock', 'lock', $lock, 'Uzamknout') ?>
      </div>
    </div>
    <div class="form-group">
      <input type='hidden' name='returnURI' value='<?= $returnURI ?>'>
      <button class="btn btn-primary"><?= $action ?></button>
      <a class="btn btn-outline-primary" href="<?= $returnURI ?>">Zpět</a>
    </div>
  </form>
</div>
