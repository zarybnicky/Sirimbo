<?php
echo $this->render('files/View/Admin/Platby/OverviewNavigation.inc');
header_main('Import plateb');
header_minor('Určení požadovaných sloupců');

echo '<form action="" method="post">';
echo $this->table()
	->style('width:100%')
	->column('column', 'Název sloupce', '', 'text-align:right;')
	->column('specific', 'Specifický<br/>symbol', 'center')
	->column('variable', 'Variabilní<br/>symbol', 'center')
	->column('date', 'Datum platby', 'center')
	->column('amount', 'Částka', 'center')
	->data($this->data);
echo '<button name="action" value="confirm">Odeslat</button>';
echo '</form>';
?>