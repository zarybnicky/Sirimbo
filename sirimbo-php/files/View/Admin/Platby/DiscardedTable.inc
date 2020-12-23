<?php \Render::fragment('files/View/Admin/Platby/Navigation.inc') ?>

<div class="container">
  <?= (new TableHelper($data))->columns($columns) ?>
</div>
