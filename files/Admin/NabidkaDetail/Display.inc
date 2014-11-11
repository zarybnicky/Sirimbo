<?= $this->header('Správa nabídky') ?>
<div class="container full">
    <?= $this->notice($this->redirect()->getMessage()); ?>
    <form action="" method="POST" class="centerWTable">
        <button type="submit" name="action" value="add" style="display:none;">Enter</button>
        <div class="n_wrapper">
            <?= $this->render('files/View/Member/Nabidka/Header.inc', $this->nabidka) ?>
            <?php
            $this->items[] = array(
                'user' => $this->userSelect()
                               ->users($this->users)
                               ->type('par')
                               ->idVar('p_id')
                               ->name('add_partner'),
                'lessonCount' => '<input type="text" name="add_hodiny" value=""  size=1/>',
                'removeButton' => '<button type="submit" name="action" value="add">Přidat</button>'
            );
            echo $this->table()
                      ->data($this->items)
                      ->column('user', 'Tanečník')
                      ->column('lessonCount', 'Počet hodin')
                      ->column('removeButton', '');
            ?>
        </div>
        <?= $this->submit('Uložit');?>
        <a href="<?= Request::getReferer();?>">Zpět</a>
    </form>
</div>
