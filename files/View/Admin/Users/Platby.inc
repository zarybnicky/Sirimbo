<?php
header_main('Správa uživatelů');
header_minor('Platby uživatele ' . $this->fullName);
notice($this->redirect()->getRedirectMessage());

echo $this->menu()
	->float(MenuHelper::FLOAT_RIGHT)
	->content('Přidat', '/admin/platby/add?u=' . $this->id)
	->content('Zpět', '/admin/users');

echo '<table>';
echo '<tr>';
echo '<td>Skupina</td>';
echo '<td>Kč/měsíc</td>';
echo '<td>Částka</td>';
echo '<td>Placeno</td>';
echo '<td>Platí do</td>';
echo '<td></td>';
echo '</tr>';

foreach($this->platby as $item) {
	echo "<tr>";
	echo '<td>', $item['colorBox'], '</td>';
	echo '<td>', $item['mesicne'], '</td>';
	echo '<td>', $item['castka'], '</td>';
	echo '<td>', $item['datePlaceno'], '</td>';
	echo '<td>', $item['datePlatnost'], '</td>';
	echo '<td>', $item['upravitLink'], '</td>';
	echo '</tr>';
}
echo '</table>';
?>