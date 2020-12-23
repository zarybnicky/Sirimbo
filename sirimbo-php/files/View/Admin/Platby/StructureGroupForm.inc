<?php new \RenderHelper('files/View/Admin/Platby/Navigation.inc', [], true) ?>

<div class="container full">
<form action="" method="post">
<?= (new \TableHelper([
        [
            'desc' => 'Typ:',
            'value' => (
                (new \RadioHelper('type', '0', $type == '0')) ->label('Běžné platby')
                . '<br/>' .
                (new \RadioHelper('type', '1', $type == '1'))->label('Členské příspěvky')
            )
        ], [
            'desc' => 'Jméno:',
            'value' => new \TextHelper('name', $name)
        ], [
            'desc' => 'Popis: ',
            'value' => '<textarea class="form-control" rows="10" cols="50" name="description">' . $description . '</textarea>'
        ], [
            'desc' => 'Násobitel částky: ',
            'value' => new \TextHelper('base', $base)
        ]
    ]))
    ->showHeader(false)
    ->column('desc', '', 'r')
    ->column('value', 'Hodnota') ?>
    <br>
    <h3>Specifické symboly</h3>
    <?= (new \TableHelper($category))
             ->column('buttons', '')
             ->column('name', 'Jméno')
             ->column('specific', 'Specifický<br/>symbol')
             ->column('amount', 'Očekávaná<br/>částka')
             ->column('dueDate', 'Splatnost')
             ->column('validDate', 'Platnost')
             ->column('usePrefix', 'Používá<br/>prefix roku')
             ->column('useBase', 'Používá<br/>násobitel částky')
             ->column('archive', 'Je neaktivní') ?>
    <br/>
    <h3>Skupiny</h3>
    <?= (new \TableHelper($skupiny))
             ->column('buttons', '')
             ->column('name', 'Jméno') ?>
    <br/>
    <?= new \HiddenHelper('returnURI', $returnURI) ?>
    <?= (new \SubmitHelper('Uložit'))->data('action', 'confirm') ?>
    <a class="btn btn-outline-primary" href="<?= $returnURI ?>">Zpět</a><br/>
</form>
</div>
