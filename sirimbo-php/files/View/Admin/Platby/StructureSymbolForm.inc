<?php \Render::fragment('files/View/Admin/Platby/Navigation.inc') ?>

<div class="container">
<form action="" method="post">

<?= (new TableHelper([
    [
        'desc' => 'Jméno:',
        'value' => new \TextHelper('name', $name)
    ], [
        'desc' => 'Specifický symbol:',
        'value' => new \TextHelper('symbol', $symbol)
    ], [
        'desc' => 'Očekávaná částka:',
        'value' => new \TextHelper('amount', $amount)
    ], [
        'desc' => 'Splatnost:',
        'value' => new \DateHelper('dueDate', $dueDate)
    ], [
        'desc' => 'Platnost:',
        'value' => (new \DateHelper('validRange', $validRange))->range()
    ], [
        'desc' => 'Povolit prefix roku?:',
        'value' => new \CheckboxHelper('usePrefix', '1', $usePrefix)
    ], [
        'desc' => 'Archivovat?:',
        'value' => new \CheckboxHelper('archive', '1', $archive)
    ], [
        'desc' => 'Viditelný:',
        'value' => new \CheckboxHelper('visible', '1', $visible)
    ]
]))
->showHeader(false)
->column('desc', '', 'r')
->column('value', 'Hodnota') ?>
    <?= new \HiddenHelper('returnURI', $returnURI) ?>
    <br/>
    <h3>Kategorie</h3>
    <?= (new TableHelper($groups))
             ->column('buttons', '')
             ->column('name', 'Jméno')
             ->column('type', 'Typ')
             ->column('base', 'Násobitel částky') ?>
    <br>
    <?= new \SubmitHelper('Uložit') ?>
    <a class="btn btn-outline-primary" href="<?= $returnURI ?>">Zpět</a><br/>
</form>
</div>
