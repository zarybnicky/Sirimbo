<?= $this->render('files/View/Admin/Platby/Navigation.inc', ['uri' => $this->uri]) ?>

<div class="container">
  <div class="row flex-wrap">
    <?php foreach ($this->columns as $col) : ?>
      <div class="col-12 col-md-6">
        <?php foreach ($col as $skupina) : ?>
          <?= $this->table()
                 ->style("width:100%;margin-bottom:.5rem")
                 ->data($skupina['users'])
                 ->column('index', '', 'r', 'width:40px')
                 ->column('fullName', $skupina['info']['header'])
                 ->column('hasPaid', 'Zaplaceno?', '', 'width:40px')
                 ->column('amount', 'Částka', '', 'width:200px') ?>
        <?php endforeach ?>
      </div>
    <?php endforeach ?>
  </div>
</div>