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
        <?php foreach ($this->meta as $meta): ?>
          <meta <?= $meta ?> />
        <?php endforeach; ?>
        <meta property="fb:app_id" content="132983570203245"/>

        <link rel="shortcut icon" type="image/x-icon" href="/favicon.ico" />
        <link rel="stylesheet" type="text/css" href="/style/polyfill.object-fit.css" />
        <link rel="stylesheet" type="text/css" href="/style/video.min.css" />
        <link rel="stylesheet" type="text/css" href="/style/bootstrap.min.css" />
        <link rel="stylesheet" type="text/css" href="/style/leaflet.css" />
        <link rel="stylesheet" type="text/css" href="/style/style.css" />
        <title>
            <?= $this->html_title ? "{$this->html_title} | TK Olymp Olomouc" : 'TK Olymp Olomouc' ?>
        </title>
    </head>
    <body>
      <div id="fb-root"></div>
      <nav class="navbar navbar-expand-md navbar-dark sticky-top">
        <div class="container">
          <h1 class="navbar-brand my-0 py-0 ml-lg-2 mx-3">
            <a href="/"><img alt="" src="/style/logo-wide-white.png" /></a>
          </h1>
          <button class="navbar-toggler" type="button" data-toggle="collapse" data-target="#nav">
            <span class="navbar-toggler-icon"></span>
          </button>
          <div class="collapse navbar-collapse ml-lg-0 ml-3" id="nav">
            <div class="flex-column" style="flex-grow:1">
              <?php foreach ($this->navbar as $row): ?>
                <ul class="navbar-nav">
                  <?php foreach ($row as $item): ?>
                    <?= $this->renderNavbarItem($item) ?>
                  <?php endforeach ?>
                </ul>
              <?php endforeach ?>
            </div>
            <?= $this->login(); ?>
          </div>
        </div>
      </nav>

      <?php if ($notice = $this->redirect()->getMessage()): ?>
        <div class="container"><div class="row"><?= $this->notice($notice); ?></div></div>
      <?php endif; ?>

