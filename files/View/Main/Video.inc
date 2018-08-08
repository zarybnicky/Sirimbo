<?= $this->header('Video') ?>

<style>
 .video-item .thumb {
     float:left;
     height:90px;
     margin-right:8px;
 }
 .video-item .info {
     padding-top:13px
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
    <?php foreach ($this->data as $item): ?>
        <div style="clear:both" class="video-item">
            <div class="thumb">
                <a href="#" class="no-a" data-id="<?= $item['uri'] ?>">
                    <img src="<?= $item['thumbnail'] ?>" style="width:150px" />
                </a>
            </div>
            <div class="info">
                <h3>
                    <a href="#" data-id="<?= $item['uri'] ?>">
                        <?= $item['name'] ?>
                    </a>
                </h3>
                <?= formatDate($item['date']) ?>
            </div>
        </div>
    <?php endforeach; ?>
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
     jQuery('.video-item a').click(function () {
         jQuery('#video-popup').append('<div><iframe src="' + $(this).data('id') + '" frameborder="0" allowfullscreen></iframe></div>').show();
     });
 })();
</script>
