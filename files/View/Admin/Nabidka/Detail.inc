<div class="container">
  <form action="" method="POST">
    <button type="submit" name="action" value="add" style="display:none;">Enter</button>
    <div class="n_wrapper">
      <?= $this->render('files/View/Member/Nabidka/Header.inc', $this->nabidka) ?>
      <?= $this->table()
             ->data($this->items)
             ->column('user', 'Tanečník')
             ->column('lessonCount', 'Počet hodin')
             ->column('removeButton', '');
      ?>
    </div>
    <?= $this->submit('Uložit') ?>
    <a class="btn btn-outline-primary" href="/admin/nabidka">Zpět</a>
  </form>
</div>
