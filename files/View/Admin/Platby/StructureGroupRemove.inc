<?= $this->render('files/View/Admin/Platby/Navigation.inc');?>
<?= $this->subheader('Kategorie plateb') ?>

<div class="container full">
    <h3>Odstranit kategorii?</h3>
    <?= $this->notice($this->redirect()->getMessage());?>

    <form action="" method="POST">
        Opravdu chcete odstranit kategorii plateb "<?= $this->name;?>"?
        <input type="hidden" name="data[]" value="<?= $this->id;?>" />
        <?= getReturnInput();?>
        <br/>
        <button type="submit" name="action" value="confirm">Odstranit</button>
        <a href="<?= $this->backlink ?>">Zpět</a>
    </form>
</div>
