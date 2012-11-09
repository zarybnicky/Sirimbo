<?php
if(NABOR && $_SERVER['REQUEST_URI'] == '/')
	View::redirect('/nabor');

notice(View::getRedirectMessage());
?>

<div>
<?php echo Helper::get()->clanky()->number(5)->slideBox(true);?>
<br/>
<div style="float:right;">
<?php echo Helper::get()->zpravy()->number(20);?>
</div>
<div style="margin-right:265px;">
<?php echo Helper::get()->clanky()->number(20)->offset(5)->slideBox(false);?>
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