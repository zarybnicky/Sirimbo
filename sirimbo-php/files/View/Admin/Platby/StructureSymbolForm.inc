<?php \Render::fragment('files/View/Admin/Platby/Navigation.inc') ?>

<div class="container">
<form action="" method="post">

<?= (new TableHelper([
    [
        'desc' => 'Jméno:',
        'value' => \Utils::text('name', $name)
    ], [
        'desc' => 'Specifický symbol:',
        'value' => \Utils::text('symbol', $symbol)
    ], [
        'desc' => 'Očekávaná částka:',
        'value' => \Utils::text('amount', $amount)
    ], [
        'desc' => 'Splatnost:',
        'value' => new \DateHelper('dueDate', $dueDate)
    ], [
        'desc' => 'Platnost:',
        'value' => (new \DateHelper('validRange', $validRange))->range()
    ], [
        'desc' => 'Povolit prefix roku?:',
        'value' => \Utils::checkbox('usePrefix', '1', $usePrefix)
    ], [
        'desc' => 'Archivovat?:',
        'value' => \Utils::checkbox('archive', '1', $archive)
    ], [
        'desc' => 'Viditelný:',
        'value' => \Utils::checkbox('visible', '1', $visible)
    ]
]))
->showHeader(false)
->column('desc', '', 'r')
->column('value', 'Hodnota') ?>
    <?= \Utils::hidden('returnURI', $returnURI) ?>
    <br/>
    <h3>Kategorie</h3>
    <?= (new TableHelper($groups))
             ->column('buttons', '')
             ->column('name', 'Jméno')
             ->column('type', 'Typ')
             ->column('base', 'Násobitel částky') ?>
    <br>
    <?= \Utils::submit('Uložit') ?>
    <a class="btn btn-outline-primary" href="<?= $returnURI ?>">Zpět</a><br/>
</form>
</div>
