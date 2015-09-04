<?= $this->render('files/View/Admin/Platby/Navigation.inc', array('uri' => $this->uri)) ?>
<?= $this->subheader($this->action == 'add' ? 'Přidat specifický symbol' : 'Upravit specifický symbol') ?>

<div class="container full">

<?php
$mainForm = $this->table()
->showHeader(false)
->column('desc', '', 'r')
->column('value', 'Hodnota')
->data(array(
    array(
        'desc' => 'Jméno:',
        'value' => '<input type="text" name="name" value="' . $this->name . '"/>'
    ), array(
        'desc' => 'Specifický symbol:',
        'value' => '<input type="text" name="symbol" value="' . $this->symbol . '"/>'
    ), array(
        'desc' => 'Očekávaná částka:',
        'value' => '<input type="text" name="amount" value="' . $this->amount . '"/>'
    ), array(
        'desc' => 'Splatnost:',
        'value' => $this->date('dueDate', $this->dueDate)
    ), array(
        'desc' => 'Platnost:',
        'value' => $this->date('validRange', $this->validRange)->range()
    ), array(
        'desc' => 'Povolit prefix roku?:',
        'value' => $this->checkbox('usePrefix', '1')->set($this->usePrefix)->render()
    ), array(
        'desc' => 'Archivovat?:',
        'value' => $this->checkbox('archive', '1')->set($this->archive)->render()
    )
))
->render();
if (!empty($this->groups)) {
    $groupForm = $this->table()
                      ->column('buttons', '', '', 'width:90px')
                      ->column('name', 'Jméno')
                      ->column('type', 'Typ')
                      ->column('base', 'Násobitel částky')
                      ->data($this->groups)
                      ->render();
}
if (!empty($this->groupSelect)) {
    $groupSelectBox = $this->select('group')->options($this->groupSelect);
}
?>

<form action="" method="post">
    <?= $mainForm ?>
    <?= $this->hidden('referer', $this->referer) ?>
    <?= $this->submit('Uložit')->data('action', 'confirm') ?>
    <a href="<?= $this->referer ?>">Zpět</a><br/>
    <br/>
</form>
<?php if ($this->action != 'add') : ?>
    <h3>Přiřazené kategorie</h3>
    <?php if (isset($groupForm)) echo $groupForm;?>
    <form action="" method="post">
        <span class="big">Přiřadit</span>
        <a href="/admin/platby/structure/group/add?category=<?= $this->id;?>">novou kategorii</a>
        <?php if (isset($groupSelectBox)) : ?>
            nebo existující: <?= $groupSelectBox;?>
            <?= $this->submit('Přiřadit')->data('action', 'group') ?>
        <?php endif;?>
    </form>
<?php endif;?>
</div>
