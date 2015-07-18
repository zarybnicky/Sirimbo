<?= $this->render('files/View/Admin/Platby/Navigation.inc') ?>
<?= $this->subheader('Kategorie plateb') ?>

<div class="container full">
<?= '<h3>', ($this->action == 'add' ? 'Přidat kategorii' : 'Upravit kategorii'), '</h3>';?>

<?php
$mainForm = $this->table()
    ->column('desc', '', 'r')
    ->column('value', 'Hodnota')
    ->data(array(
        array(
                'desc' => 'Typ:',
                'value' => '<label>' . $this->radio('type', '0')->set(true) . 'Běžné platby</label><br/>' .
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
    $categoryForm = $this->table()
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
    $categorySelectBox = $this->select('category')
        ->options($this->categorySelect);
}
if (!empty($this->skupiny)) {
    $skupinyForm = $this->table()
        ->column('buttons', '', '', 'width:75px')
        ->column('name', 'Jméno')
        ->data($this->skupiny);
}
if (!empty($this->skupinySelect)) {
    $skupinySelectBox = $this->select('skupiny')
        ->options($this->skupinySelect);
}
?>

<form action="" method="post">
    <?= $mainForm ?>
    <input type="hidden"
           name="referer"
           value="<?= $this->referer ?>" />
    <button name="action" value="confirm">Uložit</button>
    <a href="<?= $this->referer ?>">Zpět</a><br/><br/>
</form>
<?php if ($this->action != 'add') : ?>
    <h3>Specifické symboly</h3>
    <?php if (isset($categoryForm)) echo $categoryForm;?>
    <form action="" method="post">
        <span class="big">Přidat:</span>
        <a href="/admin/platby/structure/category/add?group=<?= $this->id;?>">nový specifický symbol</a>
        <?php if (isset($categorySelectBox)) : ?>
            nebo existující: <?= $categorySelectBox;?>
            <button type="submit" name="action" value="category">odeslat</button>
        <?php endif;?>
    </form>
    <br/>
    <h3>Skupiny</h3>
    <?php if (isset($skupinyForm)) echo $skupinyForm;?>
    <form action="" method="post">
        <span class="big">Přidat:</span>
        <a href="/admin/skupiny/add?group=<?= $this->id;?>">novou skupinu</a>
        <?php if (isset($skupinySelectBox)) : ?>
            nebo existující: <?= $skupinySelectBox;?>
            <button type="submit" name="action" value="skupiny">odeslat</button>
        <?php endif;?>
    </form>
<?php endif;?>
</div>
