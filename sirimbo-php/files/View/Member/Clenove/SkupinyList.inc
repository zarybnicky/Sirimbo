<?php new \RenderHelper('files/View/Member/Clenove/Navigation.inc', ['uri' => $uri], true) ?>
<div class="container">
<?= (new TableHelper($data))
    ->column('header', 'Název')
    ->column('userCount', 'Počet členů')
    ->column('description', 'Popis') ?>
</div>
