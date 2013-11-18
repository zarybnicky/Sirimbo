<?php TKOlomouc\Utility\Miscellaneous::notice(TKOlomouc\Utility\Response::getMessage()); ?>

<div style="width:710px;">

<?php echo (new TKOlomouc\View\Helper\Clanky())->count(5)->setSlideshow(true);?>
<br/>
<div style="float:right;">
    <div style="text-align:left;margin: 0 auto;">
        <?php echo (new TKOlomouc\View\Helper\Zpravy())->setCount(20);?>
    </div><br/><br/>

    <span class="little">Podporuje nás:</span>

    <div style="text-align:center;"><a href="http://www.kr-olomoucky.cz/" target="_blank">
        <img alt="Olomoucký kraj" src="/style/kraj-logo.gif" />
    </a></div>
    <div style="text-align:center;"><a href="http://www.olomouc.eu/" target="_blank">
        <img alt="Město Olomouc" src="/style/olomouc-logo.jpg" />
    </a></div>
    <br/>
    <span class="little" style="line-height:200%;">Mediální partner:<br/></span>

    <iframe src="http://poutaky.denik.cz/olomoucky.denik.cz-300x300-zpravy_region.html"
        style="width:300px;height:300px;border:0;overflow:auto;"></iframe>
</div>
<div style="margin-right:305px;">
<?php echo (new TKOlomouc\View\Helper\Clanky())->count(9)->offset(5)->setSlideshow(false);?>
</div>

</div>