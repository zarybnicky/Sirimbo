<?php new \RenderHelper('files/View/Admin/Platby/Navigation.inc', ['uri' => $this->uri]) ?>

<div class="container">
    <?php
    $form = [
        [
            'desc' => 'Datum:',
            'value' => new \DateHelper('date', $this->guess['date']),
            'recogColumn' => $this->recognized['date']['column'],
            'recogValue' => $this->recognized['date']['value']
        ], [
            'desc' => 'Částka (Kč):',
            'value' => new \TextHelper('amount', $this->guess['amount']),
            'recogColumn' => $this->recognized['amount']['column'],
            'recogValue' => $this->recognized['amount']['value']
        ], [
            'desc' => 'Uživatel: ',
            'value' => new \SelectHelper('variable', $this->users, $this->guess['variable']),
            'recogColumn' => $this->recognized['variable']['column'],
            'recogValue' => $this->recognized['variable']['value']
        ], [
            'desc' => 'Kategorie:',
            'value' => new \SelectHelper('specific', $this->categories, $this->guess['specific']),
            'recogColumn' => $this->recognized['specific']['column'],
            'recogValue' => $this->recognized['specific']['value']
        ], [
            'desc' => 'Prefix (rok):',
            'value' => new \TextHelper('prefix', $this->guess['prefix']),
            'recogColumn' => '',
            'recogValue' => $this->recognized['prefix']['value']
        ]
    ];
    $formTable = (new TableHelper($form))
                      ->column('desc', '', 'r')
                      ->column('value', 'Hodnota')
                      ->column('recogColumn', 'Rozpoznáno:<br/>sloupec')
                      ->column('recogValue', 'Rozpoznáno:<br/>hodnota');
    ?>
    <form action="" method="post">
        <?= $formTable ?>
        <?= new \HiddenHelper('id', $this->id) ?>
        <?= (new \SubmitHelper('Uložit'))->data('action', 'confirm') ?>&nbsp;
        <?= (new \SubmitHelper('Odstranit'))->data('action', 'discard') ?>&nbsp;
        <?= (new \SubmitHelper('Přeskočit'))->data('action', 'skip') ?>
    </form>
    <br/>
    <h3>Data z banky</h3>
    <?= (new TableHelper($this->raw))
              ->style('width:100%')
              ->showHeader(false)
              ->column('column', '', 'r', 'padding-right:5px')
              ->column('value', '', '', 'min-width:150px;padding-left:5px') ?>
</div>
