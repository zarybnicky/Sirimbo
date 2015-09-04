<?= $this->render('files/View/Admin/Platby/Navigation.inc', array('uri' => $this->uri)) ?>
<?= $this->subheader($this->action == 'add' ? 'Přidat kategorii' : 'Upravit kategorii') ?>

<div class="container full">

<?php
$mainForm = $this->table()
    ->showHeader(false)
    ->column('desc', '', 'r')
    ->column('value', 'Hodnota')
    ->data(array(
        array(
            'desc' => 'Typ:',
            'value' => (
                $this->radio('type', '0')
                     ->set($this->type == '0')
                     ->label('Běžné platby')
                . '<br/>' .
                $this->radio('type', '1')
                     ->set($this->type == '1')
                     ->label('Členské příspěvky')
            )
        ), array(
            'desc' => 'Jméno:',
            'value' => '<input type="text" name="name" value="' . $this->name . '"/>'
        ), array(
            'desc' => 'Popis: ',
            'value' => '<textarea rows="10" cols="50" name="description">' . $this->description . '</textarea>'
        ), array(
            'desc' => 'Násobitel částky: ',
            'value' => '<input type="text" name="base" value="' . $this->base . '"/>'
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
    $categorySelectBox = $this->select('category')->options($this->categorySelect);
}
if (!empty($this->skupiny)) {
    $skupinyForm = $this->table()
        ->column('buttons', '', '', 'width:75px')
        ->column('name', 'Jméno')
        ->data($this->skupiny);
}
if (!empty($this->skupinySelect)) {
    $skupinySelectBox = $this->select('skupiny')->options($this->skupinySelect);
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
    <h3>Specifické symboly</h3>
    <?php if (isset($categoryForm)) echo $categoryForm;?>
    <form action="" method="post">
        <span class="big">Přidat:</span>
        <a href="/admin/platby/structure/category/add?group=<?= $this->id;?>">nový specifický symbol</a>
        <?php if (isset($categorySelectBox)) : ?>
            nebo existující: <?= $categorySelectBox;?>
            <?= $this->submit('Odeslat')->data('action', 'category') ?>
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
            <?= $this->submit('Odeslat')->data('action', 'skupiny') ?>
        <?php endif;?>
    </form>
<?php endif;?>
</div>
