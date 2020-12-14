<?php new \RenderHelper('files/View/Admin/Platby/Navigation.inc', ['uri' => $this->uri]) ?>

<div class="container">
    <a href="/admin/platby/structure/group/add">Přidat kategorii</a>&nbsp;
    <?= (new TableHelper($this->data))
             ->style('width:100%')
             ->column('buttons', '', '', 'width:50px')
             ->column('name', 'Název')
             ->column('type', 'Typ platby') ?>
</div>
