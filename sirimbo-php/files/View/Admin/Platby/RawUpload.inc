<?php \Render::fragment('files/View/Admin/Platby/Navigation.inc') ?>

<div class="container">
  <h3>Upload souboru CSV</h3>
  <form action="" method="POST" enctype="multipart/form-data">
    <?= new \UploadHelper('in') ?>
    <button name="action" value="upload" class="btn btn-primary">Nahrát</button>
  </form>
</div>
