<?php
header_main('Kde trénujeme:');
?>

<p>
Náš klub dlouhá léta funguje pod záštitou dvou školských institucí. 
Jsou jimi Základní škola Holečkova a Slovanské gymnázium Olomouc.
</p>

<div style="overflow:auto;">
<?php
header_minor('Fakultní základní škola Holečkova');
?>
Holečkova 10, Olomouc <br/>
<a href="http://www.zsholeckova.cz/">http://www.zsholeckova.cz/</a><br/>
<br/>
<img src="/images/trenujeme/baletak.jpg" class="f_doc_left" style="height:200px;">
<img src="/images/trenujeme/holeckova.jpg" class="f_doc_right" style="height:200px;">
</div>

<div id="holeckova" style="width:308px; height:190px;clear:both;margin:20px auto;"
	onclick="window.location.href='http://mapy.cz/s/2LgX';"></div>
	
<script type="text/javascript" src="http://api4.mapy.cz/loader.js"></script>
<script type="text/javascript">Loader.load(null, {api:"simple"});</script>
<script type="text/javascript">
    var center = SMap.Coords.fromWGS84(17.24953485424073, 49.579712952945734);
    var m = new SMap(JAK.gel("holeckova"),center, 15, 1);
   m.addMarker(center);
</script>
<br/>
<div style="overflow:auto;">
<?php
header_minor('Slovanské gymnázium Olomouc');
?>
Jiřího z Poděbrad 13, Olomouc<br/>
<a href="http://www.sgo.cz/">http://www.sgo.cz/</a><br/>
<br/>
<img src="/images/trenujeme/sgo.jpg" class="f_doc_left" style="height:200px;">
<img src="/images/trenujeme/sgo2.jpg" class="f_doc_right" style="height:200px;">
</div>

<div id="slovan" style="width:308px; height:190px;clear:both;margin:20px auto;"
	onclick="window.location.href='http://mapy.cz/s/2Lhv';"></div>
	
<script type="text/javascript" src="http://api4.mapy.cz/loader.js"></script>
<script type="text/javascript">Loader.load(null, {api:"simple"});</script>
<script type="text/javascript">
    var center = SMap.Coords.fromWGS84(17.26312917241163, 49.59533890442716);
    var m = new SMap(JAK.gel("slovan"),center, 15, 1);
   m.addMarker(center);
</script>

