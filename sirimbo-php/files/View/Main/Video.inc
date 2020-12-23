<style>
 .playlist {
     padding: .3rem .5rem;
 }
 .playlist.active {
     background-color: #eee;
     font-weight: bolder;
 }
 #video-popup {
     z-index:11;
     position:fixed;
     top:0;
     left:0;
     width:100vw;
     height:100vh;
     background:rgba(0, 0, 0, .8);
     display:-webkit-box;
     display:-webkit-flex;
     display:-ms-flexbox;
     display:flex;
     -webkit-box-pack: center;
     -webkit-justify-content: center;
     -ms-flex-pack: center;
     justify-content: center;
     -webkit-box-align: center;
     -webkit-align-items: center;
     -ms-flex-align: center;
     align-items: center;
 }
 #video-popup div {
     max-width: 80%;
     max-height: 80%;
 }
 #video-popup iframe {
     max-width: 100%;
     max-height: 100%;
     width:650px;
     height:390px;
 }
</style>

<div class="container">

  <?php foreach ($videos as $item): ?>
    <a class="row video-item" href="<?= $item['uri'] ?>">
      <div class="col-sm-auto p-0 pb-2">
        <img src="<?= $item['thumbnail'] ?>" style="width:150px" />
      </div>
      <div class="col">
        <b><?= $item['title'] ?></b><br>
        <?= \Format::timestamp($item['date'], true) ?>
      </div>
    </a>
  <?php endforeach ?>

  <?php if ($videos) : ?><h3>Seznamy videí</h3><?php endif ?>
  <?php foreach ($playlists as $item): ?>
    <div class="playlist <?= isset($playlist) && ($playlist == $item['id']) ? 'active' : '' ?>">
      <a href="/video?playlist=<?= $item['id'] ?>">
        <b><?= $item['title'] ?></b>
      </a>
    </div>
  <?php endforeach ?>

</div>
<div id="video-popup" style="display:none"></div>

<script>
 (function checkStatus() {
     if (!/interactive|complete|loaded/.test(document.readyState)) {
         setTimeout(checkStatus, 10);
         return;
     }

     jQuery('#video-popup').click(function () {
         jQuery(this).empty().hide();
     });
     jQuery('.video-item').click(function (e) {
         e.preventDefault();
         jQuery('#video-popup').append('<div><iframe src="' + $(this).attr('href') + '" frameborder="0" allowfullscreen></iframe></div>').show();
     });
 })();
</script>
