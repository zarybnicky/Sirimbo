<?php
echo $this->render('files/View/Admin/Platby/Navigation.inc');
echo '<h2>Jednotlivé platby</h2>';

if(!TISK) {
	echo '<form action="" method="get">';
	echo '<span class="big">Omezit výběr:</span><br/>';
	echo 'uživatel:&nbsp;';
	$s = $this->select()
		->get()->name('user')
		->value('all')
		->option('all', '---');
	foreach($this->users as $user)
		$s->option($user['u_id'], $user['u_prijmeni'] . ', ' . $user['u_jmeno']);
	echo $s;
	echo '&nbsp;kategorie: ';
	$s->select()
		->get()->name('category')
		->value('all')
		->option('all', '---');
	foreach($this->categories as $key => $category)
		$s->option($key, $category);
	echo $s;
	echo '<button type="submit">Odeslat</button>';
	echo '</form>';
}

echo '<form action="" method="POST">';
if(!TISK) {
	echo $this->menu()
		->float(MenuHelper::FLOAT_RIGHT)
		->content('Přidat', '/admin/platby/items/add')
		->content('Upravit', 'edit', true)
		->content('Odebrat', 'remove', true);
}
if($this->data == array()) {
	notice('Žádné takové platby');
	return;
}
echo '<div style="margin-right:150px">';
echo $this->table()
	->style('width:100%')
	->data($this->data)
	->column('checkBox', '')
	->column('fullName', 'Uživatel')
	->column('category', 'Kategorie')
	->column('date', 'Datum')
	->column('amount', 'Částka', 'r');
echo '</div>';
echo '</form>';