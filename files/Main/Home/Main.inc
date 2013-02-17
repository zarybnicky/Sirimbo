<?php
if(NABOR && $_SERVER['REQUEST_URI'] == '/')
	View::redirect('/nabor');

notice(View::getRedirectMessage());
?>
<div>
<br/><br/>
<div style="padding:12px 0 15px 20px;border:1px solid #FFB030;border-radius:15px;background-color:#FFF0CC;">
<div style="font-size:175%;font-weight:bold;margin-bottom:12px;">
	<a href="/soutez" style="color:#B56000;">VOLBA POROTY NA LIGU</a>
</div>
<div style="color:#555;font-size:110%;">Přáslavický pohár 2013 - Memoriál Dr. Miroslava Hýži
	&nbsp;&nbsp;<b>|</b>&nbsp;&nbsp;25.květen 2013
	&nbsp;&nbsp;<b>|</b>&nbsp;&nbsp;Přáslavice</div>
</div>
<br/>
<?php echo Helper::get()->clanky()->number(5)->slideBox(true);?>
<br/>
<div style="float:right;">
<?php echo Helper::get()->zpravy()->number(20);?>
</div>
<div style="margin-right:265px;">
<?php echo Helper::get()->clanky()->number(20)->offset(5)->slideBox(false);?>
</div>
</div>