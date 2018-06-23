<?= $this->render('files/View/Member/Clenove/Navigation.inc', ['uri' => $this->uri]) ?>

<div class="container full">
    <h2><?= $this->fullName;?></h2>
    E-mail: <?= $this->email;?><br/>
    Telefon: <?= $this->telefon;?><br/>
    <a class="btn btn-outline-primary" href="<?= $this->referer;?>">Zpět</a>
</div>
