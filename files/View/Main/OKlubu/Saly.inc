<?= $this->header('Kde trénujeme') ?>

<div class="container">
  <p>
    Náš klub dlouhá léta funguje pod záštitou dvou školských institucí.
    Jsou jimi Základní škola Holečkova a Slovanské gymnázium Olomouc.
  </p>

  <div style="height:190px;padding:10px 0">
    <a id="holeckova" href="http://mapy.cz/s/2LgX" style="width:308px;height:190px;float:right;">
    </a>

    <h2>Fakultní základní škola Holečkova</h2>
    Holečkova 10, Olomouc<br/>
    <a href="http://www.zsholeckova.cz/">http://www.zsholeckova.cz/</a>
  </div>

  <div style="clear:both;padding-bottom: 10px;overflow:auto;">
    <img alt="baletní sál" src="/images/trenujeme/baletak.jpg" class="f_doc_left" style="height:300px;">
    <img alt="ZŠ Holečkova" src="/images/trenujeme/holeckova.jpg" class="f_doc_right" style="height:300px;">
  </div>

  <hr style="clear:both;" />

  <div style="height:190px;padding:10px 0 10px;clear:both;">
    <a id="slovan" href="http://mapy.cz/s/2Lhv" style="width:308px;height:190px;float:right;">
    </a>

    <h2>Slovanské gymnázium Olomouc</h2>
    Jiřího z Poděbrad 13, Olomouc<br/>
    <a href="http://www.sgo.cz/">http://www.sgo.cz/</a>
  </div>

  <div style="clear:both;">
    <img alt="Slovanské gymnázium" src="/images/trenujeme/sgo.jpg" class="f_doc_left" style="height:300px;">
    <img alt="Slovanské gymnázium" src="/images/trenujeme/sgo2.jpg" class="f_doc_right" style="height:300px;">
  </div>

  <script type="text/javascript" src="http://api4.mapy.cz/loader.js"></script>
  <script type="text/javascript">Loader.load(null, {api:"simple"});</script>
  <script type="text/javascript">
   var center = SMap.Coords.fromWGS84(17.24953485424073, 49.579712952945734);
   var m = new SMap(JAK.gel("holeckova"),center, 15, 1);
   m.addMarker(center);

   var center2 = SMap.Coords.fromWGS84(17.26312917241163, 49.59533890442716);
   var m2 = new SMap(JAK.gel("slovan"), center2, 15, 1);
   m2.addMarker(center2);
  </script>
</div>

