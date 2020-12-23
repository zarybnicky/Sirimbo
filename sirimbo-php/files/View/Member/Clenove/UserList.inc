<?php new \RenderHelper('files/View/Member/Clenove/Navigation.inc', ['uri' => $uri], true) ?>
<div class="container">
  <?= (new TableHelper($data))->column('index', '', 'r')->column('fullName', 'Jméno') ?>
</div>
