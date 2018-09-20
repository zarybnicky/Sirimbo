<?= $this->header('Správa nástěnky', ($this->action == 'add' ? 'Přidat' : 'Upravit') . ' příspěvek') ?>
<div class="container">
  <form action="" method="POST">
    <div class="form-group">
      <label for="nadpis">Nadpis</label>
      <?= $this->text('nadpis', $this->nadpis) ?>
    </div>
    <div class="form-group">
      <label for="text">Text</label>
      <textarea name="text" id="text" rows="30" cols="60" style="width:99%">
        <?= $this->text ?>
      </textarea>
      <script src="/scripts/ckeditor/ckeditor.js"></script>
      <script>
       CKEDITOR.config.allowedContent = true;
       CKEDITOR.replace('text');
      </script>
    </div>
    <div class="form-check">
      <?= $this->checkbox('lock')->label('Uzamknout')->set($this->lock) ?>
    </div>
    <div class="form-group">
      <label>Skupiny</label>
      <?php
      foreach ($this->skupiny as $item) {
          echo $this->checkbox('sk-' . $item['s_id'], '1')
                   ->set($this->skupinySelected[$item['s_id']])
                   ->label($this->colorbox($item['s_color_rgb'], $item['s_description'])
                        . '&nbsp;' . $item['s_name']);
          echo '<br>';
      }
      ?>
    </div>
    <div class="form-group">
      <?= $this->hidden('referer', $this->returnURI) ?>
      <?= $this->submit($this->action == 'add' ? 'Přidat' : 'Upravit') ?>
      <a class="btn btn-outline-primary" href="<?= $this->returnURI ?>">Zpět</a>
    </div>
  </form>
</div>
