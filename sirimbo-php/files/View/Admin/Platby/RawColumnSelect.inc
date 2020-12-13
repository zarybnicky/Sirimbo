<?= $this->render('files/View/Admin/Platby/Navigation.inc', ['uri' => $this->uri]) ?>

<h3>Určení požadovaných sloupců</h3>
<div class="container">
  <form action="" method="post">
<?= (new TableHelper($this->data))
    ->style('width:100%')
    ->column('column', 'Název sloupce', '', 'text-align:right;')
    ->column('specific', 'Specifický<br/>symbol', 'center')
    ->column('variable', 'Variabilní<br/>symbol', 'center')
    ->column('date', 'Datum platby', 'center')
    ->column('amount', 'Částka', 'center') ?>
    <button name="action" value="confirm">Odeslat</button>
  </form>
</div>
