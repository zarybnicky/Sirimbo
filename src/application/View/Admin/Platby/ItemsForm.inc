<?php
echo $this->render('src/application/View/Admin/Platby/Navigation.inc');
echo '<h2>Jednotlivé platby</h2>';
echo '<h3>', ($this->action == 'add' ? 'Přidat platbu' : 'Upravit platbu'), '</h3>';

$form = array(
    array(
        'desc' => 'Datum:',
        'value' => (new \TKOlomouc\View\Helper\Date('date'))->textBox()
    ), array(
        'desc' => 'Částka (Kč):',
        'value' => '<input type="text" name="amount" value="' . post('amount') . '"/>'
    ), array(
        'desc' => 'Uživatel: ',
        'value' => (new TKOlomouc\View\Helper\Select('variable'))->options($this->users)
    ), array(
        'desc' => 'Specifický symbol:',
        'value' => (new TKOlomouc\View\Helper\Select('specific'))->options($this->categories)
    ), array(
        'desc' => 'Prefix (rok):',
        'value' => '<input type="text" name="prefix" value="' . post('prefix') . '"/>'
    )
);
$formTable = (new TKOlomouc\View\Helper\Table())
    ->column('desc', '', 'r')
    ->column('value', 'Hodnota')
    ->data($form);
?>
<form action="" method="post">
    <?php echo $formTable, getReturnInput();?>
    <button name="action" value="confirm">Uložit</button>&nbsp;<a href="<?= $this->referer ?>">Zpět</a>
</form>
<br/>
<?php
if (!empty($this->raw)) {
    echo '<h3>Data z banky</h3>';
    echo (new TKOlomouc\View\Helper\Table())
        ->style('width:100%')
        ->showHeader(false)
        ->column('column', '', 'r', 'padding-right:5px')
        ->column('value', '', '', 'min-width:150px;padding-left:5px')
        ->data($this->raw);
}
?>