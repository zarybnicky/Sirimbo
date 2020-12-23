<?php \Render::fragment('files/View/Admin/Platby/Navigation.inc') ?>

<div class="container">
    <?php
    $form = [
        [
            'desc' => 'Datum:',
            'value' => new \DateHelper('date', $guess['date']),
            'recogColumn' => $recognized['date']['column'],
            'recogValue' => $recognized['date']['value']
        ], [
            'desc' => 'Částka (Kč):',
            'value' => new \TextHelper('amount', $guess['amount']),
            'recogColumn' => $recognized['amount']['column'],
            'recogValue' => $recognized['amount']['value']
        ], [
            'desc' => 'Uživatel: ',
            'value' => new \SelectHelper('variable', $users, $guess['variable']),
            'recogColumn' => $recognized['variable']['column'],
            'recogValue' => $recognized['variable']['value']
        ], [
            'desc' => 'Kategorie:',
            'value' => new \SelectHelper('specific', $categories, $guess['specific']),
            'recogColumn' => $recognized['specific']['column'],
            'recogValue' => $recognized['specific']['value']
        ], [
            'desc' => 'Prefix (rok):',
            'value' => new \TextHelper('prefix', $guess['prefix']),
            'recogColumn' => '',
            'recogValue' => $recognized['prefix']['value']
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
        <?= new \HiddenHelper('id', $id) ?>
        <?= (new \SubmitHelper('Uložit'))->data('action', 'confirm') ?>&nbsp;
        <?= (new \SubmitHelper('Odstranit'))->data('action', 'discard') ?>&nbsp;
        <?= (new \SubmitHelper('Přeskočit'))->data('action', 'skip') ?>
    </form>
    <br/>
    <h3>Data z banky</h3>
    <?= (new TableHelper($raw))
              ->style('width:100%')
              ->showHeader(false)
              ->column('column', '', 'r', 'padding-right:5px')
              ->column('value', '', '', 'min-width:150px;padding-left:5px') ?>
</div>
