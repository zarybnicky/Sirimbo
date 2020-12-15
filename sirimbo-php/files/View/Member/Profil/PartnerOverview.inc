<div class="container">
    <?php
    echo new PartnerRequestHelper();
    if ($this->havePartner) {
        echo "Právě tančím s: ", $this->partnerFullName;
    } else {
        echo 'Právě s nikým netančím';
    }
    ?>
    <form method="POST" action="">
        <?= new UserSelectHelper($this->users, 'partner') ?>
        <?= (new \SubmitHelper('Požádat o partnerství'))->data('action', 'confirm') ?>
        <?php if ($this->havePartner) : ?>
            <?= (new \SubmitHelper('Rozejít se'))->data('action', 'dumpthem') ?>
        <?php endif ?>
        <a class="btn btn-outline-primary" href="/member/profil">Zpět</a>
    </form>
</div>
