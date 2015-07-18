<?= $this->header('Správa skupin') ?>
<?= $this->subheader($this->header) ?>
<div class="container full">
    <?= $this->notice($this->redirect()->getMessage());?>
    <form action="" method="POST">
        <?= $this->table()
                 ->style('width:100%')
                 ->showHeader(false)
                 ->column('desc', '', 'r')
                 ->column('value', '')
                 ->data(
                     array(
                         array(
                             'desc' => 'Jméno',
                             'value' => '<input type="text" name="name" value="' . $this->name . '" />'
                         ), array(
                             'desc' => 'Barva',
                             'value' => $this->colorselect('color')->set($this->color)
                         ), array(
                             'desc' => 'Popis',
                             'value' => '<textarea rows="15" cols="50" name="desc">' . $this->popis . '</textarea>'
                         )
                     )
                 ); ?>
        <button type="submit" name="action" value="confirm">Uložit</button>
        <a href="/admin/skupiny">Zpět</a>
    </form>
    <br/>
    <?php if ($this->action != 'add'): ?>
        <?= '<h2>Kategorie plateb</h2>' ?>
        <?php if ($this->groups): ?>
            <?= $this->table()
                     ->column('buttons', '', '', 'width:90px')
                     ->column('type', 'Typ')
                     ->column('name', 'Jméno')
                     ->column('base', 'Násobitel částky')
                     ->data($this->groups) ?>
        <?php endif; ?>
        <form action="" method="post">
            <span class="big">Přidat:</span>
            <a href="/admin/platby/structure/group/add?skupina=<?= $this->id ?>">
                novou kategorii</a>
            <?php if ($this->groupSelect): ?>
                nebo existující:
                <?= $this->select('group')
                         ->options($this->groupSelect) ?>
                <button type="submit" name="action" value="group">odeslat</button>
            <?php endif;?>
        </form>
    <?php endif;?>
</div>
