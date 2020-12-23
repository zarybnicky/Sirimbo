<div class="container full">
    <form action="" method="POST">
        <?php \Render::fragment('files/View/Member/Akce/Header.inc', $data) ?>
        <?= (new \TableHelper($documents))
                 ->column('name', 'Jméno<br/>souboru')
                 ->column('category', 'Kategorie')
                 ->column('removeButton', '') ?>
        <a class="btn btn-outline-primary" href="/admin/akce">Zpět</a>
    </form>
</div>
