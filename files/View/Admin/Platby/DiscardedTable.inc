<?= $this->render('files/View/Admin/Platby/Navigation.inc', ['uri' => $this->uri]) ?>
<?= $this->subheader('Vyřazené platby (' . $this->header . ')') ?>

<div class="container">
  <?= $this->table()->data($this->data)->columns($this->columns) ?>
</div>
