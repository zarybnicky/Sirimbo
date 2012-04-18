<div>
<?php $c = new ClankyHelper(); echo $c->clanky()->number(5)->slideBox(true);?>
<br/>
<div style="float:right;">
<?php $z = new ZpravyHelper(); echo $z->zpravy()->number(20);?>
</div>
<div style="margin-right:265px;">
<?php echo $c->clanky()->number(20)->offset(5)->slideBox(false);?>
</div>
</div>

<!--
<div style="text-align:center;margin:0 auto;font-size:150%;">
Přejeme Vám<br/>
pohodové tancování<br/>
v roce 2012
</div>
<img alt="" src="/images/pernik.jpg" style="margin:25px auto;display:block;wth:350px;" />
-->