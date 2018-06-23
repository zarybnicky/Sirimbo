<?= $this->header('Správa akcí') ?>
<div class="container full">
    <form action="" method="POST" class="centerWTable">
        <button type="submit" name="action" value="enter" style="display:none;">Enter</button>
        <?= $this->render('files/View/Member/Akce/Header.inc', $this->data);?>
        <?= $this->table()
                 ->data($this->items)
                 ->column('name', 'Uživatel')
                 ->column('removeButton', '') ?>
        <?= $this->submit('Uložit')->data('action', 'submit') ?>
        <a href="/admin/akce">Zpět</a>
    </form>
</div>
