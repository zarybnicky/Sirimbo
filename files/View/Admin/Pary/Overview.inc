<?= $this->header('Správa párů') ?>
<div class="container full">
<?php
echo $this->notice($this->redirect()->getMessage());

echo '<form action="" method="POST">';
if ($this->showMenu) {
    echo $this->menu()
        ->float(MenuHelper::FLOAT_RIGHT)
        ->content('Upravit', 'edit', true)
        ->content('Odebrat', 'remove', true);
}
echo '<div style="margin-right:150px">';
echo $this->table()
    ->style('width:100%')
    ->data($this->data)
    ->column('checkBox', '')
    ->column('fullNameMan', 'Partner')
    ->column('fullNameWoman', 'Partnerka')
    ->column('standart', 'Standart')
    ->column('latina', 'Latina')
    ->column('hodnoceni', 'Body');

echo '<br/>';
echo '<h2>Nový pár</h2>';
echo 'Partner: ', $this->userSelect('add_partner')->users($this->usersMen), '<br/>';
echo 'Partnerka: ', $this->userSelect('add_partnerka')->users($this->usersWomen), '<br/>';
echo '<button type="submit" name="action" value="add">Nový pár</button>';
echo '</div>';
echo '</form>';
?>
</div>
