<?php new \RenderHelper('files/View/Member/Clenove/Navigation.inc', ['uri' => $this->uri]) ?>
<div class="container">
  <?= (new TableHelper($this->data))->column('index', '', 'r')->column('fullName', 'Jméno') ?>
</div>
