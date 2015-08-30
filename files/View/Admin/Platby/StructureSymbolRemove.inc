<?= $this->render('files/View/Admin/Platby/Navigation.inc', array('uri' => $this->uri)) ?>
<?= $this->subheader('Kategorie plateb') ?>

<div class="container full">
    <h3>Odstranit specifický symbol?</h3>
    <?= $this->notice($this->redirect()->getMessage());?>

    <form action="" method="POST">
        Opravdu chcete odstranit specifický symbol "<?= $this->name;?>"?
        <input type="hidden" name="data[]" value="<?= $this->id;?>" />
        <input type="hidden"
               name="referer"
               value="<?= $this->referer ?>" />
        <br/>
        <?= $this->submit('Odstranit')->data('action', 'confirm') ?>
        <a href="<?= $this->referer ?>">Zpět</a>
    </form>
</div>
