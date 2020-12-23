<?php new \RenderHelper('files/View/Admin/Platby/Navigation.inc', ['uri' => $uri], true) ?>

<div class="container">
  <?= (new TableHelper($data))->columns($columns) ?>
</div>
