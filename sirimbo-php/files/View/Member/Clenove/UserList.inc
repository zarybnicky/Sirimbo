<?php \Render::fragment('files/View/Member/Clenove/Navigation.inc') ?>
<div class="container">
  <?= (new TableHelper($data))->column('index', '', 'r')->column('fullName', 'Jméno') ?>
</div>
