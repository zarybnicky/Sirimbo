<div class="container">
  <ul class="nav nav-tabs">
    <li class="nav-item">
      <a class="nav-link <?= $action == 'orphan' ? 'active' : '' ?>"
         style="color:#303C3C!important" href="/admin/video/orphan">Jednotlivá videa</a>
    </li>
    <li class="nav-item">
      <a class="nav-link <?= $action == 'playlist' ? 'active' : '' ?>"
         style="color:#303C3C!important" href="/admin/video/playlist">Playlisty</a>
    </li>
    <li class="nav-item">
      <a class="nav-link" style="color:#303C3C!important" href="/admin/video/source">Zdroje</a>
    </li>
    <li class="nav-item">
      <a class="nav-link" style="color:#303C3C!important" href="/admin/video/title">Titulní stránka</a>
    </li>
  </ul>
  <a class="btn btn-primary" href="/admin/video/add">Nové video</a>
  <form action="" method="post">
    <?php if ($data) : ?>
      <?= (new TableHelper($data))
             ->style('width:100%')
             ->column('buttons', '', null, 'width:55px')
             ->column('title', 'Jméno')
             ->column('uri', 'ID videa', null)
             ->column('created', 'Přidáno', null, 'width:100px') ?>
    <?php else : ?>
      <?= new NoticeHelper('Žádná videa') ?>
    <?php endif ?>
  </form>
  <div style="text-align:center"><?= $navigation ?></div>
</div>
