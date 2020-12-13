<?= new \RenderHelper('files/View/Admin/Platby/Navigation.inc', ['uri' => $this->uri]) ?>

<div class="container">
<?php
echo '<h3>', ($this->action == 'add' ? 'Přidat platbu' : 'Upravit platbu'), '</h3>';
$form = [
    [
        'desc' => 'Datum:',
        'value' => $this->date('date', $this->date)
    ], [
        'desc' => 'Částka (Kč):',
        'value' => $this->text('amount', $this->amount)
    ], [
        'desc' => 'Uživatel: ',
        'value' => $this->select('variable')
                        ->options($this->users)
                        ->set($this->variable)
    ], [
        'desc' => 'Specifický symbol:',
        'value' => $this->select('specific')
                        ->options($this->categories)
                        ->set($this->specific)
    ], [
        'desc' => 'Prefix (rok):',
        'value' => $this->text('prefix', $this->prefix)
    ]
];
$formTable = (new TableHelper($form))
    ->column('desc', '', 'r')
    ->column('value', 'Hodnota');
?>
<form action="" method="post">
    <?= $formTable ?>
    <?= new \HiddenHelper('returnURI', $this->returnURI) ?>
    <button class="btn btn-primary" name="action" value="confirm">Uložit</button>
    <a class="btn btn-outline-primary" href="<?= $this->returnURI ?>">Zpět</a>
</form>
<br/>
<?php
if (!empty($this->raw)) {
    echo '<h3>Data z banky</h3>';
    echo (new TableHelper($this->raw))
        ->style('width:100%')
        ->showHeader(false)
        ->column('column', '', 'r', 'padding-right:5px')
        ->column('value', '', '', 'min-width:150px;padding-left:5px');
}
?>
</div>
