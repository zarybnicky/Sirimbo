<div class="container">
  <form action="" method="POST">
    <?= $this->table()
           ->style('width:100%')
           ->showHeader(false)
           ->column('desc', '', 'r')
           ->column('value', '')
           ->data(
               [
                   [
                       'desc' => 'Jméno',
                       'value' => $this->text('name', $this->name)
                   ], [
                       'desc' => 'Barva',
                       'value' => $this->colorSelect('color', $this->color)
                   ], [
                       'desc' => 'Popis',
                       'value' => '<textarea rows="15" cols="50" name="desc">' . $this->popis . '</textarea>'
                   ]
               ]
           ) ?>
    <br>
    <h2>Kategorie plateb</h2>
    <?= $this->table()
           ->column('buttons', '', '', 'width:90px')
           ->column('type', 'Typ')
           ->column('name', 'Jméno')
           ->column('base', 'Násobitel částky')
           ->data($this->groups) ?>

    <div class="form-group">
      <?= $this->submit('Uložit') ?>
      <a class="btn btn-outline-primary" href="/admin/skupiny">Zpět</a>
    </div>
  </form>
</div>
