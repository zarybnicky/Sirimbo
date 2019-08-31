<!DOCTYPE HTML>
<html prefix="og:http://ogp.me/ns" lang="cs">
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">

    <meta name="keywords" content="taneční klub, tk, olymp, olomouc, tk olymp, sportovní tanec" />
    <meta name="wot-verification" content="ec0cf41ab42dae52d3d4"/>
    <meta name="ICBM" content="49.591700,17.285174"/>
    <meta name="geo.placename" content="Olomouc, Česká Republika"/>
    <meta name="geo.position" content="49.591700;17.285174"/>
    <meta name="geo.region" content="cs"/>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
    <meta name="msvalidate.01" content="7BD6C8B5748FC22EF06AB3AE89900885" />
    <meta name="google-site-verification" content="Hfe7zlgTDOIpJv4rKGQz2Xg8Aezb6sIO0aAxVhrml9w" />
    <meta name="norton-safeweb-site-verification"
          content="r44xj2vskhlgkyqcqm1hdgga2jdfj-idvyys0277y96s72k-tq0z-yyjdu7h3el6pi2gek0i4ykq3xgiguufrvuhj8nbj4n4miwjhvumhp35jfrafyynhj4ee8ctzpzh" />
    <?php foreach ($this->meta as $meta) : ?>
      <meta <?= $meta ?> />
    <?php endforeach ?>
    <meta property="fb:app_id" content="132983570203245"/>

    <link rel="shortcut icon" type="image/x-icon" href="/favicon.ico" />
    <link rel="stylesheet" type="text/css" href="/style.css" />
    <title>
      <?= isset($this->header) ? "{$this->header} | TK Olymp Olomouc" : 'TK Olymp Olomouc' ?>
    </title>
  </head>
  <body>
    <div id="fb-root"></div>

    <nav class="navbar navbar-expand-md navbar-dark bg-dark sticky-top">
      <div class="container">
        <h1 class="navbar-brand my-0 py-0 ml-lg-2 mx-3">
          <a href="/"><img alt="" src="/style/logo-wide-white.png" /></a>
        </h1>
        <button class="navbar-toggler ml-auto" type="button" data-toggle="collapse" data-target="#nav">
          <span class="navbar-toggler-icon"></span>
        </button>
        <div class="collapse navbar-collapse ml-lg-0 ml-3" id="nav">
          <div class="flex-column" style="flex-grow:1">
            <?php $first = true; foreach ($this->navbar as $row) : ?>
              <ul class="navbar-nav <?= $first ? '' : 'd-none d-md-flex' ?>">
                <?php foreach ($row as $item) : ?>
                  <?= $this->renderNavbarItem($item) ?>
                <?php endforeach ?>
              </ul>
            <?php $first = false; endforeach ?>
          </div>
          <?= $this->login() ?>
        </div>
      </div>
    </nav>

    <?php if (isset($this->header)) : ?>
      <div class="header-section">
        <div class="container full">
          <h1><?= $this->header ?></h1>
          <?php if (isset($this->subheader)) : ?>
            <h2><?= $this->subheader ?></h2>
          <?php endif ?>
        </div>
      </div>
    <?php endif ?>

    <?php foreach ($this->redirect()->getMessages() as list($text, $type)) : ?>
      <div class="container"><?= $this->notice($text, $type) ?></div>
    <?php endforeach ?>

    <?= $this->content ?>

    <br/>
    <div id="footer">
      <div class="container">
        <div class="row" style="text-align:center">
          <div class="col" style="display:flex;flex-direction:column;justify-content:space-around;">
            <div>
              <a class="no-a" href="https://www.facebook.com/tkolymp"
                 target="_blank">
                <img alt="Facebook, stránka Taneční klub Olymp Olomouc"
                     src="/style/fb-logo.png" />
              </a>
            </div>
            <div>
              design a realizace:
              <a href="mailto:jakub@zarybnicky.com">Jakub Zárybnický</a>
              <br>
              © 2018 TK Olymp Olomouc
            </div>
          </div>
          <div class="col">
            <span>Podporuje nás:</span>
            <div>
              <a class="no-a" href="http://www.kr-olomoucky.cz/" target="_blank">
                <img alt="Olomoucký kraj" src="/style/logo-kraj.png" />
              </a>
              <br/>
              <a class="no-a" href="http://www.olomouc.eu/" target="_blank">
                <img alt="Město Olomouc" src="/style/logo-olomouc.jpg"
                     style="height:85px;width:auto;"/>
              </a>
            </div>
          </div>
        </div>
      </div>
    </div>

    <script type="text/javascript" src="/scripts/jquery-3.2.1.min.js"></script>
    <script type="text/javascript" src="/scripts/popper.min.js"></script>
    <script type="text/javascript" src="/scripts/bootstrap.min.js"></script>
    <script type="text/javascript" src="/scripts/polyfill.object-fit.min.js"></script>
    <script type="text/javascript" src="/scripts/stickyfill.min.js"></script>
    <script type="text/javascript" src="/scripts/leaflet.js"></script>
    <script type="text/javascript" src="/scripts/video.min.js"></script>
    <script type="text/javascript" src="/scripts/select2.full.js"></script>
    <script type="text/javascript" src="/scripts/main.js"></script>
    <script async="true" type="text/javascript"
            src="//connect.facebook.net/cs_CZ/all.js#xfbml=1&appId=132983570203245"
            id="facebook-jssdk"></script>
    <script type="text/javascript">
     var _gaq = _gaq || [];
     _gaq.push(['_require','inpage_linkid','//www.google-analytics.com/plugins/ga/inpage_linkid.js'],
               ['_setAccount','UA-44456908-1'],['_trackPageview']);
     (function() {
         var ga = document.createElement('script');
         ga.type='text/javascript';
         ga.async=true;
         ga.src='//www.google-analytics.com/ga.js';
         var s=document.getElementsByTagName('script')[0];
         s.parentNode.insertBefore(ga,s);
     })();
    </script>
  </body>
</html>
