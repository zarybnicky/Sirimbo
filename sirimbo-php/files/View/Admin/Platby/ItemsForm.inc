<?php new \RenderHelper('files/View/Admin/Platby/Navigation.inc', [], true) ?>

<div class="container">
<?php
echo '<h3>', ($action == 'add' ? 'Přidat platbu' : 'Upravit platbu'), '</h3>';
$form = [
    [
        'desc' => 'Datum:',
        'value' => new \DateHelper('date', $date)
    ], [
        'desc' => 'Částka (Kč):',
        'value' => new \TextHelper('amount', $amount)
    ], [
        'desc' => 'Uživatel: ',
        'value' => new \SelectHelper('variable', $users, $variable)
    ], [
        'desc' => 'Specifický symbol:',
        'value' => new \SelectHelper('specific', $categories, $specific)
    ], [
        'desc' => 'Prefix (rok):',
        'value' => new \TextHelper('prefix', $prefix)
    ]
];
$formTable = (new TableHelper($form))
    ->column('desc', '', 'r')
    ->column('value', 'Hodnota');
?>
<form action="" method="post">
    <?= $formTable ?>
    <?= new \HiddenHelper('returnURI', $returnURI) ?>
    <button class="btn btn-primary" name="action" value="confirm">Uložit</button>
    <a class="btn btn-outline-primary" href="<?= $returnURI ?>">Zpět</a>
</form>
<br/>
<?php
if (!empty($raw)) {
    echo '<h3>Data z banky</h3>';
    echo (new TableHelper($raw))
        ->style('width:100%')
        ->showHeader(false)
        ->column('column', '', 'r', 'padding-right:5px')
        ->column('value', '', '', 'min-width:150px;padding-left:5px');
}
?>
</div>
