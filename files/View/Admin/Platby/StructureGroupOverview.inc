<?= $this->render('files/View/Admin/Platby/Navigation.inc', ['uri' => $this->uri]) ?>

<div class="container">
    <a href="/admin/platby/structure/group/add">Přidat kategorii</a>&nbsp;
    <?= $this->table()
             ->style('width:100%')
             ->column('buttons', '', '', 'width:50px')
             ->column('name', 'Název')
             ->column('type', 'Typ platby')
             ->data($this->data) ?>
</div>
