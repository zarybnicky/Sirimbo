<div class="container">
    <?php
    echo new PartnerRequestHelper();
    if ($havePartner) {
        echo "Právě tančím s: ", $partnerFullName;
    } else {
        echo 'Právě s nikým netančím';
    }
    ?>
    <form method="POST" action="">
        <?= new UserSelectHelper($users, 'partner') ?>
        <?= (new \SubmitHelper('Požádat o partnerství'))->data('action', 'confirm') ?>
        <?php if ($havePartner) : ?>
            <?= (new \SubmitHelper('Rozejít se'))->data('action', 'dumpthem') ?>
        <?php endif ?>
        <a class="btn btn-outline-primary" href="/member/profil">Zpět</a>
    </form>
</div>
