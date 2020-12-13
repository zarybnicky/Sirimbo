<?= $this->render('files/View/Admin/Platby/Navigation.inc', ['uri' => $this->uri]) ?>

<div class="container">
  <?= (new TableHelper($this->data))->columns($this->columns) ?>
</div>
