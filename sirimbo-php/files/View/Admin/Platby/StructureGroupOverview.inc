<?php new \RenderHelper('files/View/Admin/Platby/Navigation.inc', ['uri' => $uri], true) ?>

<div class="container">
    <a href="/admin/platby/structure/group/add">Přidat kategorii</a>&nbsp;
    <?= (new TableHelper($data))
             ->style('width:100%')
             ->column('buttons', '', '', 'width:70px')
             ->column('name', 'Název')
             ->column('type', 'Typ platby') ?>
</div>
