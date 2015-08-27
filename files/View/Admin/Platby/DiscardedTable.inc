<?= $this->render('files/View/Admin/Platby/Navigation.inc', array('uri' => $this->uri)) ?>
<?= $this->subheader('Vyřazené platby (' . $this->header . ')') ?>

<div class="container full">
    <?= $this->table()
             ->data($this->data)
             ->columns($this->columns) ?>
</div>
