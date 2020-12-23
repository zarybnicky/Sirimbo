<div class="container">
  <form action="" method="post">
    <div class="form-group">
      <label for="uri">
        ID videa (např. <code>https://www.youtube.com/watch?v=<b>lt6h7Fgxohs</b></code>)<br>
        Ostatní pole se později zjistí z YouTube, pokud zůstanou prázdná.
      </label>
      <?= \Utils::text('uri', $uri) ?>
    </div>
    <div class="form-group">
      <label for="title">Jméno</label>
      <?= \Utils::text('title', $title) ?>
    </div>
    <div class="form-group">
      <label for="author">Autor/kanál</label>
      <?= \Utils::text('author', $author) ?>
    </div>
    <div class="form-group">
      <label for="desc">Popis</label>
      <?= \Utils::text('desc', $desc) ?>
    </div>
    <div class="form-group">
      <label for="playlist">ID playlistu (nepovinné)</label>
      <?= \Utils::text('playlist', $playlist) ?>
    </div>
    <div class="form-group">
      <?= \Utils::submit($action) ?>
      <a class="btn btn-outline-primary" href="/admin/video">Zpět</a>
    </div>
  </form>
</div>
