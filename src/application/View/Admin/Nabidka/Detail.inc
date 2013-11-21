<h1>Správa nabídky</h1>
<?php
notice($this->redirect()->getMessage());
?>
<form action="" method="POST" class="centerWTable">
    <button type="submit" name="action" value="add" style="display:none;">Enter</button>
    <div class="n_wrapper">
        <?php $this->render('src/application/View/Admin/Nabidka/Header.inc', $this->data) ?>
        <table>
            <tr>
                <td>Tanečník</td>
                <td>Počet hodin</td>
            </tr>
            <?php
            $userselect = (new TKOlomouc\View\Helper\UserSelect())
                ->users($this->users)
                ->type('par')
                ->idVar('p_id');
            foreach ($this->items as $item) :
                post($item['id'] . '-partner', $item['partner']);
            ?>
                <tr>
                    <td>
                        <?= $userselect->name($item['id'] . '-partner'); ?>
                    </td>
                    <td>
                        <input type="text"
                            name="<?= $item['id'] ?>-hodiny"
                            value="<?= $item['pocetHod'] ?>"
                            size="1" />
                    </td>
                    <td>
                        <button type="submit" name="remove" value="<?= $item['id'] ?>">
                            Odstranit
                        </button>
                    </td>
                </tr>
            <?php endforeach; ?>
            <tr>
                <td>
                    <?php echo $userselect->name('add_partner');?>
                </td>
                <td>
                    <input type="text" name="add_hodiny" value="" size=1/>
                </td>
                <td>
                    <button type="submit" name="action" value="add">Přidat</button>
                </td>
            </tr>
            <tr>
                <td>
                    <button type="submit" name="action" value="submit">Uložit</button>
                </td>
            </tr>
        </table>
    </div>
    <a href="<?= $this->action ?>">Zpět</a>
</form>