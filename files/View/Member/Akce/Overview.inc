<?= $this->header('Klubové akce') ?>

<div class="container full">
  <?= $this->notice($this->redirect()->getMessage()) ?>

  <style type="text/css">
   .unit {
       background:inherit;border:none;
       vertical-align:top;border-bottom:1px dotted #999;padding:10px 5px;
       width:50%;
   }
   .unit form {text-align:center;}
  </style>
  <table style="width:100%;">
    <?php foreach (array_chunk($this->akce, 2) as $chunk) : ?>
      <tr>
        <?php foreach ($chunk as $data) : ?>
          <td class="unit">
            <form action="" method="post">
              <?= $this->render('files/View/Member/Akce/Header.inc', $data);?>
              <?php if ($data['showForm']) : ?>
                <input type="hidden" name="id" value="<?= $data['id'];?>" />
                <?php if ($data['signIn']) : ?>
                  <?= $this->submit('Přihlásit se')->data('action', 'signup') ?> &bull;
                <?php else: ?>
                  <?= $this->submit('Odhlásit se')->data('action', 'signout') ?> &bull;
                <?php endif; ?>
              <?php endif; ?>
              <a href="/member/akce/<?= $data['id'] ?>">Zobrazit přihlášené</a>
            </form>
          </td>
        <?php endforeach; ?>
      </tr>
    <?php endforeach; ?>
  </table>
</div>
