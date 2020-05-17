<div class="container">
  <div class="row flex-wrap flex-space-around">
    <?php foreach ($this->akce as $item): ?>
      <div class="col-12 col-lg-6 pb-2 text-center">
        <form action="" method="post">
          <?= $this->render('files/View/Member/Akce/Header.inc', $item) ?>
          <?= $this->hidden('id', $item['id']) ?>
          <?php if ($item['signOut']) : ?>
            <?= $this->submit('Odhlásit se')->cls('btn btn-outline-primary btn-sm')->data('action', 'signout') ?>
          <?php endif ?>
          <?php if ($item['signIn']) : ?>
            <?= $this->submit('Přihlásit se')->cls('btn btn-outline-primary btn-sm')->data('action', 'signup') ?>
          <?php endif ?>
          <a href="/member/akce/<?= $item['id'] ?>" class="btn btn-outline-primary btn-sm">
            Zobrazit přihlášené
          </a>
        </form>
      </div>
    <?php endforeach ?>
  </div>
</div>
