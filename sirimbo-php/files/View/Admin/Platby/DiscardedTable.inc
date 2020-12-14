<?php new \RenderHelper('files/View/Admin/Platby/Navigation.inc', ['uri' => $this->uri], true) ?>

<div class="container">
  <?= (new TableHelper($this->data))->columns($this->columns) ?>
</div>
