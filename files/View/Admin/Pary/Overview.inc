<?= $this->header('Správa párů') ?>

<div class="container">
    <form action="" method="POST">
        <?php
        if ($this->showMenu) {
            echo $this->menu()
                      ->content('Upravit', 'edit', true)
                      ->content('Odebrat', 'remove', true)
                      ->content('Opravit nespárované', 'fix_unpaired', true);
        }
        ?>

        <div<?= $this->showMenu ? ' style="margin-right:150px"' : '' ?>>
            <?= $this->table()
                     ->style('width:100%')
                     ->data($this->data)
                     ->column('checkBox', '')
                     ->column('fullNameMan', 'Partner')
                     ->column('fullNameWoman', 'Partnerka')
                     ->column('standard', 'Standard')
                     ->column('latina', 'Latina')
                     ->column('hodnoceni', 'Body');
            ?>
            <br/>
            <h2>Nový pár</h2>
            Partner: <?= $this->userSelect('add_partner')->users($this->usersMen) ?><br/>
            Partnerka: <?= $this->userSelect('add_partnerka')->users($this->usersWomen) ?><br/>
            <?= $this->submit('Nový pár')->data('action', 'add') ?>
        </div>
    </form>
</div>
