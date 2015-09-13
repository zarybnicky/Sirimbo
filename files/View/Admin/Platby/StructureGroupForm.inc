<?= $this->render('files/View/Admin/Platby/Navigation.inc', array('uri' => $this->uri)) ?>
<?= $this->subheader($this->action == 'add' ? 'Přidat kategorii' : 'Upravit kategorii') ?>

<div class="container full">
<form action="" method="post">
<?= $this->table()
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
    )) ?>
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
    <?= $this->hidden('referer', $this->referer) ?>
    <?= $this->submit('Uložit')->data('action', 'confirm') ?>
    <a href="<?= $this->referer ?>">Zpět</a><br/>
</form>
</div>
