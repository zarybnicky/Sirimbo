<?php new \RenderHelper('files/View/Admin/Platby/Navigation.inc', ['uri' => $uri], true) ?>

<div class="container">
  <div class="row flex-wrap">
    <?php foreach ($columns as $col) : ?>
      <div class="col-12 col-md-6">
        <?php foreach ($col as $skupina) : ?>
          <?= (new TableHelper($skupina['users']))
                 ->style("width:100%;margin-bottom:.5rem")
                 ->column('index', '', 'r', 'width:40px')
                 ->column('fullName', $skupina['info']['header'])
                 ->column('hasPaid', 'Zaplaceno?', '', 'width:40px')
                 ->column('amount', 'Částka', '', 'width:200px') ?>
        <?php endforeach ?>
      </div>
    <?php endforeach ?>
  </div>
</div>
