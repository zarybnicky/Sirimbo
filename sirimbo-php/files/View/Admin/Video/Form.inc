<div class="container">
  <form action="" method="post">
    <div class="form-group">
      <label for="uri">
        ID videa (např. <code>https://www.youtube.com/watch?v=<b>lt6h7Fgxohs</b></code>)<br>
        Ostatní pole se později zjistí z YouTube, pokud zůstanou prázdná.
      </label>
      <?= $this->text('uri', $this->uri) ?>
    </div>
    <div class="form-group">
      <label for="title">Jméno</label>
      <?= $this->text('title', $this->title) ?>
    </div>
    <div class="form-group">
      <label for="author">Autor/kanál</label>
      <?= $this->text('author', $this->author) ?>
    </div>
    <div class="form-group">
      <label for="desc">Popis</label>
      <?= $this->text('desc', $this->desc) ?>
    </div>
    <div class="form-group">
      <label for="playlist">ID playlistu (nepovinné)</label>
      <?= $this->text('playlist', $this->playlist) ?>
    </div>
    <div class="form-group">
      <?= $this->submit($this->action) ?>
      <a class="btn btn-outline-primary" href="/admin/video">Zpět</a>
    </div>
  </form>
</div>