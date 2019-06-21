<div class="container">
  <div class="row flex-wrap flex-space-around">
    <?php foreach ($this->akce as $item): ?>
      <div class="col-12 col-lg-6 pb-2 text-center">
        <form action="" method="post">
          <?= $this->render('files/View/Member/Akce/Header.inc', $item) ?>
          <?php if ($item['showForm']) : ?>
            <?= $this->hidden('id', $item['id']) ?>
            <?= $this->submit($item['signIn'] ? 'Přihlásit se' : 'Odhlásit se')
                   ->cls('btn btn-outline-primary btn-sm')
                   ->data('action', $item['signIn'] ? 'signup' : 'signout') ?>
          <?php endif ?>
          <a href="/member/akce/<?= $item['id'] ?>" class="btn btn-outline-primary btn-sm">
            Zobrazit přihlášené
          </a>
        </form>
      </div>
    <?php endforeach ?>
  </div>
</div>
