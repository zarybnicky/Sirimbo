<?= $this->render('files/View/Admin/Platby/Navigation.inc', ['uri' => $this->uri]) ?>

<div class="container full">
<form action="" method="post">
<?= $this->table()
    ->showHeader(false)
    ->column('desc', '', 'r')
    ->column('value', 'Hodnota')
    ->data([
        [
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
        ], [
            'desc' => 'Jméno:',
            'value' => $this->text('name', $this->name)
        ], [
            'desc' => 'Popis: ',
            'value' => '<textarea class="form-control" rows="10" cols="50" name="description">' . $this->description . '</textarea>'
        ], [
            'desc' => 'Násobitel částky: ',
            'value' => $this->text('base', $this->base)
        ]
    ]) ?>
    <br>
    <h3>Specifické symboly</h3>
    <?= $this->table()
             ->column('buttons', '')
             ->column('name', 'Jméno')
             ->column('specific', 'Specifický<br/>symbol')
             ->column('amount', 'Očekávaná<br/>částka')
             ->column('dueDate', 'Splatnost')
             ->column('validDate', 'Platnost')
             ->column('usePrefix', 'Používá<br/>prefix roku')
             ->column('useBase', 'Používá<br/>násobitel částky')
             ->column('archive', 'Je neaktivní')
             ->data($this->category) ?>
    <br/>
    <h3>Skupiny</h3>
    <?= $this->table()
             ->column('buttons', '')
             ->column('name', 'Jméno')
             ->data($this->skupiny) ?>
    <br/>
    <?= $this->hidden('returnURI', $this->returnURI) ?>
    <?= $this->submit('Uložit')->data('action', 'confirm') ?>
    <a class="btn btn-outline-primary" href="<?= $this->returnURI ?>">Zpět</a><br/>
</form>
</div>