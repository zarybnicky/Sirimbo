<?= $this->render('files/View/Admin/Platby/Navigation.inc', ['uri' => $this->uri]) ?>
<?= $this->subheader('Kategorie plateb') ?>

<div class="container full">
    <h3>Odstranit specifický symbol?</h3>
    <form action="" method="POST">
        Opravdu chcete odstranit specifický symbol "<?= $this->name;?>"?
        <?= $this->hidden('data[]', $this->id) ?>
        <?= $this->hidden('referer', $this->referer) ?>
        <br/>
        <?= $this->submit('Odstranit')->data('action', 'confirm') ?>
        <a href="<?= $this->referer ?>">Zpět</a>
    </form>
</div>
