<?php new \RenderHelper('files/View/Member/Clenove/Navigation.inc', ['uri' => $this->uri], true) ?>
<div class="container">
<?= (new TableHelper($this->data))
    ->column('header', 'Název')
    ->column('userCount', 'Počet členů')
    ->column('description', 'Popis') ?>
</div>
