<?php
header_main('Správa skupin');
notice($this->redirect()->getRedirectMessage());

if($this->showMenu) {
	echo '<form action="" method="get">';
	echo 'zobrazení:&nbsp;';
	echo $this->select()
		->get()->name('v')
		->option('info', 'skupiny')
		->option('users', 'uživatelé podle skupin');
	echo '<button type="submit">Odeslat</button>';
	echo '</form><br/>';
}

if(get('v') == 'info') {
	echo '<form action="/admin/skupiny" method="POST">';
	if($this->showMenu) {
		echo $this->menu()
			->float(MenuHelper::FLOAT_RIGHT)
			->content('Přidat', 'skupiny/add')
			->content('Upravit', 'edit', true)
			->content('Odebrat', 'remove', true);
	}
	echo '<div style="margin-right:150px">';
	echo $this->table()
		->style('width:100%')
		->column('checkBox', '')
		->column('colorBox', 'Barvy', 'center')
		->column('popis', 'Popis', '', 'max-width:300px;')
		->column('platbaMesic', 'Kč/měsíc', 'center')
		->column('platbaCtvrtleti', 'Kč/čtvrtletí', 'center')
		->column('platbaPololeti', 'Kč/pololetí', 'center')
		->data($this->data);
	echo '</div>';
	echo '</form>';
} elseif(get('v') == 'users') {
	echo '<table style="width:100%;border:none;background:inherit;">';
	echo '<tr style="border:none;background:inherit;">';
	foreach($this->data as $column) {
		echo '<td style="border:none;background:inherit;vertical-align:top;">';
		foreach($column as $skupina) {
			header_minor($skupina['header']);
			if(empty($skupina['users'])) {
				echo '<br/>';
				continue;
			}
			echo '<div>';
			echo $this->table()
				->style('width:100%')
				->column('fullName', 'Jméno', '', '')
				->column('castka', 'Částka', 'center', 'width:40px;')
				->column('platbaDatum', 'Zaplaceno do', 'center', 'width:90px;')
				->data($skupina['users']);
			echo '</div>';
			echo '<br/>';
		}
		echo '</td>';
	}
	echo '</tr>';
	echo '</table>';
}
?>