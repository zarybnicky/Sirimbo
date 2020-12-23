<?php new \RenderHelper('files/View/Admin/Platby/Navigation.inc', [], true) ?>

<div class="container">
  <?= (new TableHelper($data))->columns($columns) ?>
</div>
