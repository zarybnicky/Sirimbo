<?= new \RenderHelper('files/View/Admin/Platby/Navigation.inc', ['uri' => $this->uri]) ?>

<div class="container">
<form action="" method="post">

<?= (new TableHelper([
    [
        'desc' => 'Jméno:',
        'value' => new \TextHelper('name', $this->name)
    ], [
        'desc' => 'Specifický symbol:',
        'value' => new \TextHelper('symbol', $this->symbol)
    ], [
        'desc' => 'Očekávaná částka:',
        'value' => new \TextHelper('amount', $this->amount)
    ], [
        'desc' => 'Splatnost:',
        'value' => new \DateHelper('dueDate', $this->dueDate)
    ], [
        'desc' => 'Platnost:',
        'value' => (new \DateHelper('validRange', $this->validRange))->range()
    ], [
        'desc' => 'Povolit prefix roku?:',
        'value' => new \CheckboxHelper('usePrefix', '1', $this->usePrefix)
    ], [
        'desc' => 'Archivovat?:',
        'value' => new \CheckboxHelper('archive', '1', $this->archive)
    ], [
        'desc' => 'Viditelný:',
        'value' => new \CheckboxHelper('visible', '1', $this->visible)
    ]
]))
->showHeader(false)
->column('desc', '', 'r')
->column('value', 'Hodnota') ?>
    <?= new \HiddenHelper('returnURI', $this->returnURI) ?>
    <br/>
    <h3>Kategorie</h3>
    <?= (new TableHelper($this->groups))
             ->column('buttons', '')
             ->column('name', 'Jméno')
             ->column('type', 'Typ')
             ->column('base', 'Násobitel částky') ?>
    <br>
    <?= new \SubmitHelper('Uložit') ?>
    <a class="btn btn-outline-primary" href="<?= $this->returnURI ?>">Zpět</a><br/>
</form>
</div>
