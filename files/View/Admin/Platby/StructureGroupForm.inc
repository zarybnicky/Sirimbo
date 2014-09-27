<?php
$mainForm = (new TableHelper())->table()
    ->column('desc', '', 'r')
    ->column('value', 'Hodnota')
    ->data(array(
        array(
                'desc' => 'Typ:',
                'value' => '<label>' . $this->radio('type', '0')->defaultState(true) . 'Běžné platby</label><br/>' .
                    '<label>' . $this->radio('type', '1') . 'Členské příspěvky</label>'
        ), array(
                'desc' => 'Jméno:',
                'value' => '<input type="text" name="name" value="' . post('name') . '"/>'
        ), array(
                'desc' => 'Popis: ',
                'value' => '<textarea rows="10" cols="50" name="description">' . post('description') . '</textarea>'
        ), array(
                'desc' => 'Násobitel částky: ',
                'value' => '<input type="text" name="base" value="' . post('base') . '"/>'
        )
    ));
if (!empty($this->category)) {
    $categoryForm = (new TableHelper())->table()
        ->column('buttons', '', '', 'width:75px')
        ->column('name', 'Jméno')
        ->column('specific', 'Specifický<br/>symbol')
        ->column('amount', 'Očekávaná<br/>částka')
        ->column('dueDate', 'Splatnost')
        ->column('validDate', 'Platnost')
        ->column('usePrefix', 'Používá<br/>prefix roku')
        ->column('useBase', 'Používá<br/>násobitel částky')
        ->column('archive', 'Je neaktivní')
        ->data($this->category);
}
if (!empty($this->categorySelect)) {
    $categorySelectBox = (new SelectHelper())->select('category')
        ->options($this->categorySelect);
}
if (!empty($this->skupiny)) {
    $skupinyForm = (new TableHelper())->table()
        ->column('buttons', '', '', 'width:75px')
        ->column('name', 'Jméno')
        ->data($this->skupiny);
}
if (!empty($this->skupinySelect)) {
    $skupinySelectBox = (new SelectHelper())->select('skupiny')
        ->options($this->skupinySelect);
}
?>

<?php echo $this->render('files/View/Admin/Platby/Navigation.inc');?>
<?php echo '<h2>Kategorie plateb</h2>';?>
<?php echo '<h3>', ($this->action == 'add' ? 'Přidat kategorii' : 'Upravit kategorii'), '</h3>';?>
<form action="" method="post">
    <?php echo $mainForm, getReturnInput();?>
    <button name="action" value="confirm">Uložit</button>&nbsp;<a href="<?php echo Request::getReferer();?>">Zpět</a><br/><br/>
</form>
<?php if ($this->action != 'add') : ?>
    <h3>Specifické symboly</h3>
    <?php if (isset($categoryForm)) echo $categoryForm;?>
    <form action="" method="post">
        <span class="big">Přidat:</span>
        <a href="/admin/platby/structure/category/add?group=<?php echo $this->id;?>">nový specifický symbol</a>
        <?php if (isset($categorySelectBox)) : ?>
            nebo existující: <?php echo $categorySelectBox;?>
            <button type="submit" name="action" value="category">odeslat</button>
        <?php endif;?>
    </form>
    <br/>
    <h3>Skupiny</h3>
    <?php if (isset($skupinyForm)) echo $skupinyForm;?>
    <form action="" method="post">
        <span class="big">Přidat:</span>
        <a href="/admin/skupiny/add?group=<?php echo $this->id;?>">novou skupinu</a>
        <?php if (isset($skupinySelectBox)) : ?>
            nebo existující: <?php echo $skupinySelectBox;?>
            <button type="submit" name="action" value="skupiny">odeslat</button>
        <?php endif;?>
    </form>
<?php endif;?>
