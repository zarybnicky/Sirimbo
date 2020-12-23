<div class="container">
  <form action="" method="POST">
    <div class="form-group">
      <label for="nadpis">Nadpis</label>
      <?= new \TextHelper('nadpis', $nadpis) ?>
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
      <?php
      foreach ($skupiny as $item) {
          echo '<div class="form-check">';
          echo new \CheckboxHelper(
              'sk-' . $item['s_id'], '1', $skupinySelected[$item['s_id']],
              new ColorboxHelper($item['s_color_rgb'], $item['s_description']) . '&nbsp;' . $item['s_name']
          );
          echo '</div>';
      }
      ?>
    </div>
    <br>
    <div class="form-group">
      <div class="form-check">
        <?= new \CheckboxHelper('lock', 'lock', $lock, 'Uzamknout') ?>
      </div>
    </div>
    <div class="form-group">
      <?= new \HiddenHelper('returnURI', $returnURI) ?>
      <?= new \SubmitHelper($action == 'add' ? 'Přidat' : 'Upravit') ?>
      <a class="btn btn-outline-primary" href="<?= $returnURI ?>">Zpět</a>
    </div>
  </form>
</div>
