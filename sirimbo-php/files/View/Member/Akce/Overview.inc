<div class="container">
  <div class="row flex-wrap flex-space-around">
    <?php foreach ($akce as $item): ?>
      <div class="col-12 col-lg-6 pb-2 text-center">
        <form action="" method="post">
          <?php \Render::fragment('files/View/Member/Akce/Header.inc', $item) ?>
          <?= new \HiddenHelper('id', $item['id']) ?>
          <?php if ($item['signOut']) : ?>
            <?= (new \SubmitHelper('Odhlásit se'))->cls('btn btn-outline-primary btn-sm')->data('action', 'signout') ?>
          <?php endif ?>
          <?php if ($item['signIn']) : ?>
            <?= (new \SubmitHelper('Přihlásit se'))->cls('btn btn-outline-primary btn-sm')->data('action', 'signup') ?>
          <?php endif ?>
          <a href="/member/akce/<?= $item['id'] ?>" class="btn btn-outline-primary btn-sm">
            Zobrazit přihlášené
          </a>
        </form>
      </div>
    <?php endforeach ?>
  </div>
</div>
