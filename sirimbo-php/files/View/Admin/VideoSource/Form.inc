<div class="container">
  <form action="" method="post">
    <div class="form-group">
      <label for="uri">
        ID kanálu (např. <code>https://www.youtube.com/channel/<b>UCopG139AfgpmaswNXmEwX2Q</b></code>)<br>
        Ostatní pole se později zjistí z YouTube, pokud zůstanou prázdná.
      </label>
      <?= new \TextHelper('uri', $uri) ?>
    </div>
    <div class="form-group">
      <label for="title">Jméno</label>
      <?= new \TextHelper('title', $title ?: '') ?>
    </div>
    <div class="form-group">
      <label for="desc">Popis</label>
      <?= new \TextHelper('desc', $desc ?: '') ?>
    </div>
    <div class="form-group">
      <?= new \SubmitHelper($action) ?>
      <a class="btn btn-outline-primary" href="/admin/video/source">Zpět</a>
    </div>
  </form>
</div>
