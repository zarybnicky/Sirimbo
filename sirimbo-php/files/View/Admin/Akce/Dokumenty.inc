<div class="container full">
    <form action="" method="POST">
        <?php new \RenderHelper('files/View/Member/Akce/Header.inc', $this->data, true) ?>
        <?= (new \TableHelper($this->documents))
                 ->column('name', 'Jméno<br/>souboru')
                 ->column('category', 'Kategorie')
                 ->column('removeButton', '') ?>
        <a class="btn btn-outline-primary" href="/admin/akce">Zpět</a>
    </form>
</div>
