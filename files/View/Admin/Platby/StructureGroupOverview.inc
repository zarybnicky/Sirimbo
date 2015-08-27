<?= $this->render('files/View/Admin/Platby/Navigation.inc', array('uri' => $this->uri)) ?>
<?= $this->subheader('Kategorie plateb') ?>

<div class="container full">
    <span class="big">Přidat: </span>
    <a href="/admin/platby/structure/group/add">kategorii</a>&nbsp;
    <?= $this->table()
             ->style('width:100%')
             ->column('buttons', '', '', 'width:50px')
             ->column('name', 'Název')
             ->column('type', 'Typ platby')
             ->data($this->data) ?>
</div>
