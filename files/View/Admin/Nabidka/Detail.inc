<?= $this->header('Správa nabídky') ?>
<div class="container full">
    <?= $this->notice($this->redirect()->getMessage()); ?>

    <form action="" method="POST" class="centerWTable">
        <button type="submit" name="action" value="add" style="display:none;">Enter</button>
        <div class="n_wrapper">
            <?= $this->render('files/View/Member/Nabidka/Header.inc', $this->nabidka) ?>
            <?= $this->table()
                     ->data($this->items)
                     ->column('user', 'Tanečník')
                     ->column('lessonCount', 'Počet hodin')
                     ->column('removeButton', '');
            ?>
        </div>
        <?= $this->submit('Uložit') ?>
        <a href="<?= $this->backlink ?>">Zpět</a>
    </form>
</div>
