<div class="container">
    <?php
    echo \Utils::partnerRequest();
    if ($havePartner) {
        echo "Právě tančím s: ", $partnerFullName;
    } else {
        echo 'Právě s nikým netančím';
    }
    ?>
    <form method="POST" action="">
        <?= \Utils::userSelect($users, 'partner') ?>
        <?= \Utils::submit('Požádat o partnerství', 'action', 'confirm') ?>
        <?php if ($havePartner) : ?>
            <?= \Utils::submit('Rozejít se', 'action', 'dumpthem') ?>
        <?php endif ?>
        <a class="btn btn-outline-primary" href="/member/profil">Zpět</a>
    </form>
</div>
