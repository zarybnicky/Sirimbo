<?php \Render::fragment('files/View/Member/Clenove/Navigation.inc') ?>
<div class="container">
<?= (new TableHelper($data))
    ->column('header', 'Název')
    ->column('userCount', 'Počet členů')
    ->column('description', 'Popis') ?>
</div>
