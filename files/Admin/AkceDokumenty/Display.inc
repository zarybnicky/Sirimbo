<?= $this->header('Správa akcí') ?>
<div class="container full">
    <?= $this->notice($this->redirect()->getMessage()); ?>
    <form action="" method="POST" class="centerWTable">
        <?= $this->render('files/View/Member/Akce/Header.inc', $this->data);?>
        <?= $this->table()
                 ->data($this->documents)
                 ->column('name', 'Jméno<br/>souboru')
                 ->column('category', 'Kategorie')
                 ->column('removeButton', '');?>
        <a href="/admin/akce">Zpět</a>
    </form>
</div>
