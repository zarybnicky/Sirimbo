<?= $this->render('files/View/Admin/Platby/Navigation.inc', array('uri' => $this->uri)) ?>
<?= $this->subheader($this->action == 'add' ? 'Přidat specifický symbol' : 'Upravit specifický symbol') ?>

<div class="container full">
<form action="" method="post">

<?= $this->table()
->showHeader(false)
->column('desc', '', 'r')
->column('value', 'Hodnota')
->data(array(
    array(
        'desc' => 'Jméno:',
        'value' => '<input type="text" name="name" value="' . $this->name . '"/>'
    ), array(
        'desc' => 'Specifický symbol:',
        'value' => '<input type="text" name="symbol" value="' . $this->symbol . '"/>'
    ), array(
        'desc' => 'Očekávaná částka:',
        'value' => '<input type="text" name="amount" value="' . $this->amount . '"/>'
    ), array(
        'desc' => 'Splatnost:',
        'value' => $this->date('dueDate', $this->dueDate)
    ), array(
        'desc' => 'Platnost:',
        'value' => $this->date('validRange', $this->validRange)->range()
    ), array(
        'desc' => 'Povolit prefix roku?:',
        'value' => $this->checkbox('usePrefix', '1')->set($this->usePrefix)->render()
    ), array(
        'desc' => 'Archivovat?:',
        'value' => $this->checkbox('archive', '1')->set($this->archive)->render()
    )
))
->render() ?>
    <?= $this->hidden('referer', $this->referer) ?>
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
    <?= $this->submit('Uložit')->data('action', 'confirm') ?>
    <a href="<?= $this->referer ?>">Zpět</a><br/>
</form>
</div>
