<?php \Render::fragment('files/View/Admin/Platby/Navigation.inc') ?>

<div class="container">
<h3><?= $action == 'add' ? 'Přidat platbu' : 'Upravit platbu' ?></h3>
<?php
$form = [
    [
        'desc' => 'Datum:',
        'value' => new \DateHelper('date', $date)
    ], [
        'desc' => 'Částka (Kč):',
        'value' => \Utils::text('amount', $amount)
    ], [
        'desc' => 'Uživatel: ',
        'value' => \Utils::select('variable', $users, $variable)
    ], [
        'desc' => 'Specifický symbol:',
        'value' => \Utils::select('specific', $categories, $specific)
    ], [
        'desc' => 'Prefix (rok):',
        'value' => \Utils::text('prefix', $prefix)
    ]
];
?>
<form action="" method="post">
    <?= (new TableHelper($form))
          ->column('desc', '', 'r')
          ->column('value', 'Hodnota') ?>
    <input type='hidden' name='returnURI' value='<?= $returnURI ?>'>
    <button class="btn btn-primary" name="action" value="confirm">Uložit</button>
    <a class="btn btn-outline-primary" href="<?= $returnURI ?>">Zpět</a>
</form>
<br/>
<?php if (!empty($raw)) : ?>
  <h3>Data z banky</h3>
  <?= (new TableHelper($raw))
        ->style('width:100%')
        ->showHeader(false)
        ->column('column', '', 'r', 'padding-right:5px')
        ->column('value', '', '', 'min-width:150px;padding-left:5px') ?>
<?php endif ?>
</div>
