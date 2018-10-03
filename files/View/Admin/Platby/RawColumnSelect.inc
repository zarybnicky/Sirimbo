<?= $this->render('files/View/Admin/Platby/Navigation.inc', ['uri' => $this->uri]) ?>
<?= $this->subheader('Import plateb') ?>

<h3>Určení požadovaných sloupců</h3>
<div class="container">
  <form action="" method="post">
<?= $this->table()
    ->style('width:100%')
    ->column('column', 'Název sloupce', '', 'text-align:right;')
    ->column('specific', 'Specifický<br/>symbol', 'center')
    ->column('variable', 'Variabilní<br/>symbol', 'center')
    ->column('date', 'Datum platby', 'center')
    ->column('amount', 'Částka', 'center')
    ->data($this->data) ?>
    <button name="action" value="confirm">Odeslat</button>
  </form>
</div>
