<?= $this->render('files/View/Admin/Platby/Navigation.inc', ['uri' => $this->uri]) ?>
<?= $this->subheader(
    'Ruční třídění plateb</span> (zbývá ' . $this->remainingTotal . ')'
) ?>

<div class="container full">
    <?php
    $form = [
        [
            'desc' => 'Datum:',
            'value' => $this->date('date', $this->guess['date']),
            'recogColumn' => $this->recognized['date']['column'],
            'recogValue' => $this->recognized['date']['value']
        ], [
            'desc' => 'Částka (Kč):',
            'value' => $this->text('amount', $this->guess['amount']),
            'recogColumn' => $this->recognized['amount']['column'],
            'recogValue' => $this->recognized['amount']['value']
        ], [
            'desc' => 'Uživatel: ',
            'value' => $this->select('variable')->options($this->users)->set($this->guess['variable']),
            'recogColumn' => $this->recognized['variable']['column'],
            'recogValue' => $this->recognized['variable']['value']
        ], [
            'desc' => 'Kategorie:',
            'value' => $this->select('specific')->options($this->categories)->set($this->guess['specific']),
            'recogColumn' => $this->recognized['specific']['column'],
            'recogValue' => $this->recognized['specific']['value']
        ], [
            'desc' => 'Prefix (rok):',
            'value' => $this->text('prefix', $this->guess['prefix']),
            'recogColumn' => '',
            'recogValue' => $this->recognized['prefix']['value']
        ]
    ];
    $formTable = $this->table()
                      ->column('desc', '', 'r')
                      ->column('value', 'Hodnota')
                      ->column('recogColumn', 'Rozpoznáno:<br/>sloupec')
                      ->column('recogValue', 'Rozpoznáno:<br/>hodnota')
                      ->data($form);
    ?>
    <form action="" method="post">
        <?= $formTable;?>
        <?= $this->hidden('id', $this->id) ?>
        <?= $this->submit('Uložit')->data('action', 'confirm') ?>&nbsp;
        <?= $this->submit('Odstranit')->data('action', 'discard') ?>&nbsp;
        <?= $this->submit('Přeskočit')->data('action', 'skip') ?>
    </form>
    <br/>
    <?php
    echo '<h3>Data z banky</h3>';
    echo $this->table()
              ->style('width:100%')
              ->showHeader(false)
              ->column('column', '', 'r', 'padding-right:5px')
              ->column('value', '', '', 'min-width:150px;padding-left:5px')
              ->data($this->raw);
    ?>
</div>
