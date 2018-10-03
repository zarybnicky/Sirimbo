<?= $this->render('files/View/Admin/Platby/Navigation.inc', ['uri' => $this->uri]) ?>
<?= $this->subheader('Kategorie plateb') ?>

<div class="container">
    <h3>Odstranit kategorii?</h3>
    <form action="" method="POST">
        Opravdu chcete odstranit kategorii plateb "<?= $this->name ?>"?
        <?= $this->hidden('data[]', $this->id) ?>
        <?= $this->hidden('referer', $this->referer) ?>
        <br/>
        <?= $this->submit('Odstranit')->data('action', 'confirm') ?>
        <a class="btn btn-outline-primary" href="<?= $this->referer ?>">Zpět</a>
    </form>
</div>
