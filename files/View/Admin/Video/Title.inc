<div class="container">
  <ul class="nav nav-tabs">
    <li class="nav-item">
      <a class="nav-link" style="color:#303C3C!important" href="/admin/video/orphan">Jednotlivá videa</a>
    </li>
    <li class="nav-item">
      <a class="nav-link" style="color:#303C3C!important" href="/admin/video/playlist">Playlisty</a>
    </li>
    <li class="nav-item">
      <a class="nav-link active" style="color:#303C3C!important" href="/admin/video/title">Titulní stránka</a>
    </li>
  </ul>
  <form action="" method="post">
    <?= $this->video1 ?> <?= $this->video2 ?><br>
    <?= $this->video3 ?> <?= $this->video4 ?><br>
    <?= $this->submit('Uložit') ?><br>
  </form>
</div>
