<div class="container">
    <?php
    echo $this->partnerRequest()->getAll();
    if ($this->havePartner) {
        echo "Právě tančím s: ", $this->partnerFullName;
    } else {
        echo 'Právě s nikým netančím';
    }
    ?>
    <form method="POST" action="">
        <?= $this->userSelect()
                 ->name('partner')
                 ->tmpSwitch(false)
                 ->users($this->users) ?>
        <?= $this->submit('Požádat o partnerství')->data('action', 'confirm') ?>
        <?php if ($this->havePartner): ?>
            <?= $this->submit('Rozejít se')->data('action', 'dumpthem') ?>
        <?php endif ?>
        <a class="btn btn-outline-primary" href="/member/profil/par">Zpět</a>
    </form>
</div>
