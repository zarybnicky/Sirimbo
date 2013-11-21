<h1>Správa párů</h1>
<?php
notice($this->redirect()->getMessage());

echo '<form action="" method="POST">';
if ($this->showMenu) {
    echo (new TKOlomouc\View\Helper\Menu())
        ->content('Upravit', 'edit', true)
        ->content('Odebrat', 'remove', true);
}
echo '<div style="margin-right:150px">';
echo (new TKOlomouc\View\Helper\Table())
    ->style('width:100%')
    ->column('checkBox', '')
    ->column('fullNameMan', 'Partner')
    ->column('fullNameWoman', 'Partnerka')
    ->column('standart', 'Standart')
    ->column('latina', 'Latina')
    ->column('hodnoceni', 'Body')
    ->data($this->data);

echo '<br/>';
echo '<h2>Nový pár</h2>';
echo 'Partner: ', (new TKOlomouc\View\Helper\UserSelect('add_partner'))->users($this->usersMen), '<br/>';
echo 'Partnerka: ', (new TKOlomouc\View\Helper\UserSelect('add_partnerka'))->users($this->usersWomen), '<br/>';
echo '<button type="submit" name="action" value="add">Nový pár</button>';
echo '</div>';
echo '</form>';
?>