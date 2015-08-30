<?= $this->render('files/View/Admin/Platby/Navigation.inc', array('uri' => $this->uri)) ?>
<?= $this->subheader('Jednotlivé platby') ?>

<div class="container full">
<?php
echo '<h3>', ($this->action == 'add' ? 'Přidat platbu' : 'Upravit platbu'), '</h3>';

$form = array(
        array(
                'desc' => 'Datum:',
                'value' => $this->date('date', $this->date)->textBox()
        ), array(
                'desc' => 'Částka (Kč):',
                'value' => '<input type="text" name="amount" value="' . $this->amount . '"/>'
        ), array(
                'desc' => 'Uživatel: ',
                'value' => $this->select('variable')->options($this->users)->set($this->variable)->render()
        ), array(
                'desc' => 'Specifický symbol:',
                'value' => $this->select('specific')->options($this->categories)->set($this->specific)->render()
        ), array(
                'desc' => 'Prefix (rok):',
                'value' => '<input type="text" name="prefix" value="' . $this->prefix . '"/>'
        )
);
$formTable = $this->table()
    ->column('desc', '', 'r')
    ->column('value', 'Hodnota')
    ->data($form);
?>
<form action="" method="post">
    <?= $formTable ?>
    <input type="hidden"
           name="referer"
           value="<?= $this->referer ?>" />
    <button name="action" value="confirm">Uložit</button>
    <a href="<?= $this->referer ?>">Zpět</a>
</form>
<br/>
<?php
if (!empty($this->raw)) {
    echo '<h3>Data z banky</h3>';
    echo $this->table()
        ->style('width:100%')
        ->showHeader(false)
        ->column('column', '', 'r', 'padding-right:5px')
        ->column('value', '', '', 'min-width:150px;padding-left:5px')
        ->data($this->raw);
}
?>
</div>
