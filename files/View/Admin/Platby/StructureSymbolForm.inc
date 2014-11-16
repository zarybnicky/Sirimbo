<?= $this->render('files/View/Admin/Platby/Navigation.inc');?>
<?= $this->subheader('Kategorie plateb') ?>

<div class="container full">
<?= '<h3>' . ($this->action == 'add' ? 'Přidat specifický symbol' : 'Upravit specifický symbol') . '</h3>';?>

<?php
$mainForm = $this->table()
->column('desc', '', 'r')
->column('value', 'Hodnota')
->data(array(
        array(
            'desc' => 'Jméno:',
            'value' => '<input type="text" name="name" value="' . post('name') . '"/>'
        ), array(
            'desc' => 'Specifický symbol:',
            'value' => '<input type="text" name="symbol" value="' . post('symbol') . '"/>'
        ), array(
            'desc' => 'Očekávaná částka:',
            'value' => '<input type="text" name="amount" value="' . post('amount') . '"/>'
        ), array(
            'desc' => 'Splatnost:',
            'value' => $this->date('dueDate')
        ), array(
            'desc' => 'Platnost:',
            'value' => $this->date('validRange')->range()
        ), array(
            'desc' => 'Povolit prefix roku?:',
            'value' => (string) $this->checkbox('usePrefix', '1')
        ), array(
            'desc' => 'Archivovat?:',
            'value' => (string) $this->checkbox('archive', '1')
        )
));
if (!empty($this->groups)) {
    $groupForm = $this->table()
        ->column('buttons', '', '', 'width:75px')
        ->column('type', 'Typ')
        ->column('name', 'Jméno')
        ->column('base', 'Násobitel částky')
        ->data($this->groups);
}
if (!empty($this->groupSelect)) {
    $groupSelectBox = $this->select('group')
        ->options($this->groupSelect);
}
?>

<form action="" method="post">
    <?php echo $mainForm, getReturnInput();?>
    <button name="action" value="confirm">Uložit</button>&nbsp;<a href="<?= $this->backlink ?>">Zpět</a><br/><br/>
</form>
<?php if ($this->action != 'add') : ?>
    <h3>Kategorie</h3>
    <?php if (isset($groupForm)) echo $groupForm;?>
    <form action="" method="post">
        <span class="big">Přidat:</span>
        <a href="/admin/platby/structure/group/add?category=<?= $this->id;?>">novou kategorii</a>
        <?php if (isset($groupSelectBox)) : ?>
            nebo existující: <?= $groupSelectBox;?>
            <button type="submit" name="action" value="group">odeslat</button>
        <?php endif;?>
    </form>
<?php endif;?>
</div>
