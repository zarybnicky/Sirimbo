<div id="carousel" class="carousel slide" data-ride="carousel">
  <ol class="carousel-indicators">
    <li data-target="#carousel" data-slide-to="0" class="active"></li>
    <li data-target="#carousel" data-slide-to="1"></li>
    <li data-target="#carousel" data-slide-to="2"></li>
  </ol>
  <div class="carousel-inner">
    <?php $first = true; foreach ($this->highlights as $item): ?>
      <div class="carousel-item<?php if ($first) { echo " active"; $first = false; } ?>"
          style="background-image:linear-gradient(to bottom, rgba(0, 0, 0, 0.1), rgba(0, 0, 0, 0.4)),url(<?= $item['title_photo_uri'] ?>)">
        <img src="<?= $item['title_photo_uri'] ?>" alt="First slide" />
        <div class="carousel-caption">
          <a href="<?= $item['uri'] ?>"><h3><?= $item['name'] ?></h3></a>
          <p class="d-none d-md-block"><?= $item['description'] ?></p>
        </div>
      </div>
    <?php endforeach ?>
  </div>
  <a class="carousel-control-prev" href="#carousel" role="button" data-slide="prev">
    <span class="carousel-control-prev-icon" aria-hidden="true"></span>
    <span class="sr-only">Předchozí</span>
  </a>
  <a class="carousel-control-next" href="#carousel" role="button" data-slide="next">
    <span class="carousel-control-next-icon" aria-hidden="true"></span>
    <span class="sr-only">Další</span>
  </a>
</div>

<div class="container about" style="margin-top:3.5rem;margin-bottom:3.5rem">
  <div class="row">
    <div class="col-md-6">
      <h2>O klubu</h2>
      <p>
        <b>TK Olymp</b> je tradiční taneční klub zabývající se výchovou
        standardních a&nbsp;latinskoamerických tanečníků. V&nbsp;novém tisíciletí
        patří mezi&nbsp;nejúspěšnější české kluby. Naše páry pravidelně sbírají
        úspěchy v&nbsp;České republice i&nbsp;na&nbsp;mezinárodním poli.
      </p>
      <p>
        Nabízíme atraktivní volnočasovou aktivitu dětem, kvalitní zázemí
        sportovním tanečníkům a&nbsp;příjemné prostředí pro&nbsp;všechny, co&nbsp;mají
        rádi tanec.
      </p>
    </div>
    <div class="col-md-6" style="text-align:center">
      <img class="logo-image" alt="" src="/style/logo-big.png" />
    </div>
  </div>
</div>

<div class="courses">
  <div>
    <div class="container">
      <div class="row">
        <div class="col-md-6 courses__desc" style="text-align:right">
          <h4>Taneční přípravka (5&ndash;7&nbsp;let)</h4>
          <p>
            Taneční a&nbsp;pohybové hry, seznámení s&nbsp;hudbou a&nbsp;základními kroky.
          </p>
          <p>
            Vyučuje mladý proškolený trénérský kolektiv v&nbsp;kombinaci se&nbsp;zkušenými&nbsp;učiteli.
          </p>
        </div>
        <div class="col-md-6 courses__img">
          <img alt="" src="/images/julca-mikulas.jpg" style="" />
        </div>
      </div>
    </div>
  </div>
  <div>
    <div class="container">
      <div class="row">
        <div class="col-md-6 courses__img text-right">
          <img alt="" src="/images/boruvci.jpg" style="" />
        </div>
        <div class="col-md-6 courses__desc">
          <h4>Tančení pro&nbsp;děti (8&ndash;14&nbsp;let)</h4>
          <p>
            Pohybová a&nbsp;taneční průprava, základní figury a&nbsp;techniky standardních
            a&nbsp;latinskoamerických tanců.
          </p>
          <p>
            Možné jako kroužek i&nbsp;jako příprava na&nbsp;taneční sport.
          </p>
          <p>
            Vyučuje mladý proškolený trénérský kolektiv, složený z&nbsp;mistrů České
            republiky a&nbsp;tanečníků mezinárodní třídy.
          </p>
        </div>
      </div>
    </div>
  </div>
  <div>
    <div class="container">
      <div class="row">
        <div class="col-md-6 courses__desc" style="text-align:right">
          <h4>Lekce pro&nbsp;začátečníky (15+&nbsp;let)</h4>
          <p>
            Vhodné pro všechny, kteří ještě netančili nebo prošli kurzy v&nbsp;tanečních.
          </p>
          <p>
            Vyučujeme základní figury a techniky standardních a
            latinskoamerických tanců, držení těla, souhru s&nbsp;partnerem a&nbsp;soulad
            s&nbsp;hudbou.
          </p>
          <p>
            Vyučuje mladý proškolený trénérský kolektiv, složený z&nbsp;mistrů České republiky
            a&nbsp;tanečníků mezinárodní třídy.
          </p>
        </div>
        <div class="col-md-6 courses__img">
          <img alt="" src="/images/david-verca.jpg" style="" />
        </div>
      </div>
    </div>
  </div>
  <div>
    <div class="container">
      <div class="row">
        <div class="col-md-6 courses__img text-right">
          <img alt="" src="/images/vilda-lat.jpg" style="" />
        </div>
        <div class="col-md-6 courses__desc">
          <h4>Taneční sport</h4>
          <p>
            Vhodné pro&nbsp;pokročilé tanečníky se&nbsp;soutěžními ambicemi od&nbsp;dětí
            po&nbsp;dospělé.
          </p>
          <p>
            Tréninkový plán je zaměřený na výuku techniky standardních a&nbsp;latinskoamerických
            tanců, kondiční a&nbsp;fyzickou přípravu a&nbsp;soutěžní podání tanečního
            sportu.
          </p>
          <p>
            Vyučují extérní lektoři z&nbsp;Prahy, Brna, Ostravy - trenéři nejlepších párů
            a&nbsp;mistrů České republiky.
          </p>
          <p>
            Volné tréninky pro&nbsp;vlastní samostatné trénování, možnost individuálních
            lekcí s&nbsp;extérními trenéry.
          </p>
        </div>
      </div>
    </div>
  </div>
</div>

<div class="container news">
  <div class="row" style="margin-top:3.5rem;margin-bottom:3.5rem">
    <div class="col-md-6">
      <h3><a href="/aktualne/clanky">Aktuálně</a></h3>
      <div class="row">
        <?php foreach ($this->moreArticles as $item): ?>
          <div class="col-12 col-md-6 pb-2">
            <div class="article-small card">
              <a class="card-img-top image" href="<?= $item['uri'] ?>">
                <img alt="" src="<?= $item['title_photo_uri'] ?>" />
              </a>
              <div class="card-body">
                <h4 class="card-title">
                  <a href="<?= $item['uri'] ?>"><?= $item['name'] ?></a>
                </h4>
                <p class="card-text"><?= $item['description'] ?></p>
                <div style="text-align:right;font-size:85%;">
                  <a href="<?= $item['uri'] ?>">více zde &raquo;</a>
                </div>
              </div>
            </div>
          </div>
        <?php endforeach ?>
      </div>
    </div>

    <div class="col-md-6">
      <h3><a href="/video">Videa ze soutěží</a></h3>
      <ul class="list-group">
        <?php foreach ($this->videos as $x): ?>
          <li class="list-group-item">
            <a class="video row" href="<?= $x['link'] ?>">
              <div class="col-4"><img style="max-width:100%" alt="" src="<?= $x['image'] ?>"/></div>
              <div class="col-8"><h5><?= $x['title'] ?></h5></div>
            </a>
          </li>
        <?php endforeach ?>
      </ul>
    </div>
  </div>
</div>

<div class="container contact">
  <h2>Kontakt</h2>
  <div class="row">
    <div class="col-md">
      <h3>Taneční klub</h3>
      <p>
        <b>Taneční klub Olymp Olomouc</b><br>
        Jiráskova 25, 779 00 Olomouc<br>
        IČO: 68347286<br>
        <a href="mailto:tkolymp@tkolymp.cz">tkolymp@tkolymp.cz</a>
      </p>
      <h3>Taneční sály</h3>
      <p>
        <b>Taneční centrum při FZŠ Holečkova</b><br>
        Holečkova 10, 779 00 Olomouc<br>
        (vchod brankou u&nbsp;zastávky <i>Povel, škola</i>)
      </p>
      <p>
        <b>Tělocvična Slovanského gymnázia</b><br>
        Jiřího z Poděbrad 13, 779 00 Olomouc<br>
        (vchod bránou z&nbsp;ulice <i>U reálky</i>)
      </p>
    </div>
    <div class="col-md">
      <div id="m" style="height:50vh"></div>
    </div>
  </div>
</div>