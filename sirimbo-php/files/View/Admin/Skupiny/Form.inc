<div class="container">
  <form action="" method="POST">
    <?= (new TableHelper([
        [
            'desc' => 'Jméno',
            'value' => $this->text('name', $this->name)
        ], [
            'desc' => 'Barva',
            'value' => (new ColorSelectHelper())->colorSelect('color', $this->color)
        ], [
            'desc' => 'Popis',
            'value' => '<textarea rows="15" cols="50" name="desc">' . $this->popis . '</textarea>'
        ]
    ]))
       ->style('width:100%')
       ->showHeader(false)
       ->column('desc', '', 'r')
       ->column('value', '') ?>
    <br>
    <h2>Kategorie plateb</h2>
    <?= (new TableHelper($this->groups))
           ->column('buttons', '', '', 'width:90px')
           ->column('type', 'Typ')
           ->column('name', 'Jméno')
           ->column('base', 'Násobitel částky') ?>

    <div class="form-group">
      <?= $this->submit('Uložit') ?>
      <a class="btn btn-outline-primary" href="/admin/skupiny">Zpět</a>
    </div>
  </form>
</div>
