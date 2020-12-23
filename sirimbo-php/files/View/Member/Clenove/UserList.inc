<?php new \RenderHelper('files/View/Member/Clenove/Navigation.inc', [], true) ?>
<div class="container">
  <?= (new TableHelper($data))->column('index', '', 'r')->column('fullName', 'Jméno') ?>
</div>
