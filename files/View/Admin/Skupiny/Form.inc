<?php
$mainForm = (new TableHelper())->table()
    ->style('width:100%')
    ->showHeader(false)
    ->column('desc', '', 'r')
    ->column('value', '')
    ->data(array(
            array(
                    'desc' => 'Jméno',
                    'value' => '<input type="text" name="name" value="' . post('name') . '" />'
            ), array(
                    'desc' => 'Barva',
                    'value' => $this->colorselect('color')
            ), array(
                    'desc' => 'Popis',
                    'value' => '<textarea rows="15" cols="50" name="desc">' . post('popis') . '</textarea>'
            )
    ));
    
if (!empty($this->groups)) {
    $groupForm = (new TableHelper())->table()
        ->column('buttons', '', '', 'width:75px')
        ->column('type', 'Typ')
        ->column('name', 'Jméno')
        ->column('base', 'Násobitel částky')
        ->data($this->groups);
}
if (!empty($this->groupSelect)) {
    $groupSelectBox = (new SelectHelper())->select('group')
        ->options($this->groupSelect);
}
?>
<h1>Správa skupin</h1>
<?php echo '<h2>', ($this->action == 'add' ? 'Přidat skupinu' : 'Upravit skupinu'), '</h2>';?>
<?php notice($this->redirect()->getMessage());?>
<form action="" method="POST">
    <?php echo $mainForm;?>
    <button type="submit" name="action" value="confirm">Uložit</button>&nbsp;<a href="/admin/skupiny">Zpět</a>
</form>
<br/>
<?php if ($this->action != 'add'):?>
    <?php echo '<h2>Kategorie plateb</h2>';?>
    <?php if (isset($groupForm)) echo $groupForm;?>
    <form action="" method="post">
        <span class="big">Přidat:</span>
        <a href="/admin/platby/structure/group/add?skupina=<?php echo $this->id;?>">novou kategorii</a>
        <?php if (isset($groupSelectBox)): ?>
            nebo existující: <?php echo $groupSelectBox;?>
            <button type="submit" name="action" value="group">odeslat</button>
        <?php endif;?>
    </form>
<?php endif;?>