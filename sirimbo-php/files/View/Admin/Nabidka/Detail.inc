<div class="container">
  <form action="" method="POST">
    <button type="submit" name="action" value="add" style="display:none;">Enter</button>
    <div class="n_wrapper">
      <?php Render::fragment('files/View/Member/Nabidka/Header.inc', $nabidka) ?>
      <?= (new \TableHelper($items))
             ->column('user', 'Tanečník')
             ->column('lessonCount', 'Počet hodin')
             ->column('removeButton', '');
      ?>
    </div>
    <button class="btn btn-primary">Uložit</button>
    <a class="btn btn-outline-primary" href="/admin/nabidka">Zpět</a>
  </form>
</div>
