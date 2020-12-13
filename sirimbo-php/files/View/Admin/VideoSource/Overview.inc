<div class="container">
  <ul class="nav nav-tabs">
    <li class="nav-item">
      <a class="nav-link" style="color:#303C3C!important" href="/admin/video/orphan">Jednotlivá videa</a>
    </li>
    <li class="nav-item">
      <a class="nav-link" style="color:#303C3C!important" href="/admin/video/playlist">Playlisty</a>
    </li>
    <li class="nav-item">
      <a class="nav-link active" style="color:#303C3C!important" href="/admin/video/source">Zdroje</a>
    </li>
    <li class="nav-item">
      <a class="nav-link" style="color:#303C3C!important" href="/admin/video/title">Titulní stránka</a>
    </li>
  </ul>
  <a class="btn btn-primary" href="/admin/video/source/add">Nový zdroj</a>
  <form action="" method="post">
    <?php if ($this->data) : ?>
      <?= (new TableHelper($this->data))
             ->style('width:100%')
             ->column('buttons', '', null, 'width:55px')
             ->column('title', 'Jméno')
             ->column('url', 'ID kanálu', null)
             ->column('created', 'Přidáno', null, 'width:100px')
             ->column('lastChecked', 'Poslední kontrola', null, 'width:100px') ?>
    <?php else : ?>
      <?= new NoticeHelper('Žádné zdroje') ?>
    <?php endif ?>
  </form>
</div>
