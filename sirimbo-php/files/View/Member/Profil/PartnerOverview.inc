<div class="container">
    <?php
    echo (new PartnerRequestHelper())->getAll();
    if ($this->havePartner) {
        echo "Právě tančím s: ", $this->partnerFullName;
    } else {
        echo 'Právě s nikým netančím';
    }
    ?>
    <form method="POST" action="">
        <?= new UserSelectHelper($this->users, 'partner') ?>
        <?= $this->submit('Požádat o partnerství')->data('action', 'confirm') ?>
        <?php if ($this->havePartner) : ?>
            <?= $this->submit('Rozejít se')->data('action', 'dumpthem') ?>
        <?php endif ?>
        <a class="btn btn-outline-primary" href="/member/profil/par">Zpět</a>
    </form>
</div>
