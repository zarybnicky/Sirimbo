<?php
echo $this->render('files/View/Admin/Platby/OverviewNavigation.inc');
notice($this->redirect()->getRedirectMessage());

if($this->showMenu) {
	echo '<form action="/admin/platby" method="get">';
	echo 'za období:&nbsp;';
	$s = $this->select()
		->get()->name('f')
		->option('all', 'Všechny platby', true);
	for($i = date('Y'); $i >= '2011'; $i--) {
		$s->option($i . '-2' , '2. pololetí ' . $i . '/' . ($i + 1));
		$s->option($i . '-1' , '1. pololetí ' . $i . '/' . ($i + 1));
	}
	echo $s;
	echo 'řadit podle:&nbsp;';
	echo $this->select()
		->get()->name('s')
		->option('prijmeni', 'příjmení', true)
		->option('var-symbol', 'var. symbolu')
		->option('placeno', 'data platby');
	echo '<button type="submit">Odeslat</button><br/><br/>';
	echo '</form>';
}
if($this->filter !== 'all') {
	echo 'Z celkového počtu <span class="big">' . $this->totalUsers .
		'</span> zaplatilo <span class="big">' . $this->totalPaid .  '</span> uživatelů.<br/><br/>';
}
echo '<form action="" method="POST">';
if($this->showMenu) {
	echo $this->menu()
		->float(MenuHelper::FLOAT_RIGHT)
		->content('Přidat', 'platby/add')
		->content('Upravit', 'edit', true)
		->content('Odebrat', 'remove', true);
}
if(empty($this->data)) {
	notice('Žádné platby za toto období');
	return;
}
echo '<div style="margin-right:150px">';
echo $this->table()
	->style('width:100%')
	->data($this->data)
	->column('checkBox', '')
	->column('fullName', 'Jméno')
	->column('colorBox', '')
	->column('varSymbol', 'Variabilní<br/>symbol')
	->column('obdobi', 'Období')
	->column('castka', 'Částka')
	->column('paidDate', 'Placeno')
	->column('paidUntil', 'Platí do');
echo '</div>';
echo '</form>';
echo '<div style="margin-right:15Opx;text-align:center;">', $this->navigation, '</div>';
?>