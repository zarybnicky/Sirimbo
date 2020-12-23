<div class="container">
  <form action="" method="POST">
    <?= (new TableHelper([
        [
            'desc' => 'Jméno',
            'value' => new \TextHelper('name', $name)
        ], [
            'desc' => 'Barva',
            'value' => new \ColorSelectHelper('color', $color)
        ], [
            'desc' => 'Popis',
            'value' => '<textarea rows="15" cols="50" name="desc">' . $popis . '</textarea>'
        ]
    ]))
       ->style('width:100%')
       ->showHeader(false)
       ->column('desc', '', 'r')
       ->column('value', '') ?>
    <br>
    <h2>Kategorie plateb</h2>
    <?= (new \TableHelper($groups))
           ->column('buttons', '', '', 'width:90px')
           ->column('type', 'Typ')
           ->column('name', 'Jméno')
           ->column('base', 'Násobitel částky') ?>

    <div class="form-group">
      <?= new \SubmitHelper('Uložit') ?>
      <a class="btn btn-outline-primary" href="/admin/skupiny">Zpět</a>
    </div>
  </form>
</div>
