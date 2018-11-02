<?= $this->render('files/View/Member/Clenove/Navigation.inc', ['uri' => $this->uri]) ?>

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
                 ->column('hasPaid', 'Zaplaceno?', '', 'width:40px') ?>
        <?php endforeach ?>
      </div>
    <?php endforeach ?>
  </div>
</div>
