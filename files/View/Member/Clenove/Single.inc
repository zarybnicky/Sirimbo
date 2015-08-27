<?= $this->render('files/View/Member/Clenove/Navigation.inc', array('uri' => $this->uri)) ?>

<div class="container full">
    <h2><?= $this->fullName;?></h2>
    E-mail: <?= $this->email;?><br/>
    Telefon: <?= $this->telefon;?><br/>
    <a href="<?= $this->referer;?>">Zpět</a>
</div>
