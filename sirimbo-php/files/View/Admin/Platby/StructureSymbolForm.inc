<?= $this->render('files/View/Admin/Platby/Navigation.inc', ['uri' => $this->uri]) ?>

<div class="container">
<form action="" method="post">

<?= $this->table()
->showHeader(false)
->column('desc', '', 'r')
->column('value', 'Hodnota')
->data([
    [
        'desc' => 'Jméno:',
        'value' => $this->text('name', $this->name)
    ], [
        'desc' => 'Specifický symbol:',
        'value' => $this->text('symbol', $this->symbol)
    ], [
        'desc' => 'Očekávaná částka:',
        'value' => $this->text('amount', $this->amount)
    ], [
        'desc' => 'Splatnost:',
        'value' => $this->date('dueDate', $this->dueDate)->render()
    ], [
        'desc' => 'Platnost:',
        'value' => $this->date('validRange', $this->validRange)->range()
    ], [
        'desc' => 'Povolit prefix roku?:',
        'value' => $this->checkbox('usePrefix', '1')->set($this->usePrefix)->render()
    ], [
        'desc' => 'Archivovat?:',
        'value' => $this->checkbox('archive', '1')->set($this->archive)->render()
    ], [
        'desc' => 'Viditelný:',
        'value' => $this->checkbox('visible', '1')->set($this->visible)->render()
    ]
])
->render() ?>
    <?= $this->hidden('returnURI', $this->returnURI) ?>
    <br/>
    <h3>Kategorie</h3>
    <?= $this->table()
             ->column('buttons', '')
             ->column('name', 'Jméno')
             ->column('type', 'Typ')
             ->column('base', 'Násobitel částky')
             ->data($this->groups)
             ->render() ?>
    <br>
    <?= $this->submit('Uložit') ?>
    <a class="btn btn-outline-primary" href="<?= $this->returnURI ?>">Zpět</a><br/>
</form>
</div>