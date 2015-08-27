<?= $this->render('files/View/Admin/Platby/Navigation.inc', array('uri' => $this->uri)) ?>
<?= $this->subheader('Kategorie plateb') ?>

<div class="container full">
    <h3>Odstranit kategorii?</h3>
    <?= $this->notice($this->redirect()->getMessage());?>

    <form action="" method="POST">
        Opravdu chcete odstranit kategorii plateb "<?= $this->name;?>"?
        <input type="hidden" name="data[]" value="<?= $this->id;?>" />
        <input type="hidden"
               name="referer"
               value="<?= $this->referer ?>" />
        <br/>
        <button type="submit" name="action" value="confirm">Odstranit</button>
        <a href="<?= $this->referer ?>">Zpět</a>
    </form>
</div>
