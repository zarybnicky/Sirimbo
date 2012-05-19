<?php header_main('Odkazy');?>
<ul class="list">
	<li><a href="http://www.csts.cz/">Český svaz tanečního sportu</a></li>
	<li><a href="http://www.worlddancesport.org/">World DanceSport Federation</a></li>
	<li><a href="http://www.dancesportinfo.net/">Dancesport info - The Dancers´ Website</a></li>
	<li><a href="http://tanecni-olomouc.cz/">Taneční škola Progress Olomouc</a></li>
	<li><a href="http://talidance.wz.cz/">Tali Dance - taneční obuv</a></li>
	<li><a href="http://www.henzely.cz/">Dance-point - obchody s tanečním zbožím</a></li>
	<li><a href="http://www.hellerdance.cz/">Heller Dance and Fashion</a></li>
	<li><a href="http://www.spaeker.de/">Heinz Spaeker - DanceSport Services</a></li>
	<li><a href="http://www.majkluvsvet.cz/">Majklův svět - Tanec v obrazech</a></li>
	<li><a href="http://dalibor-trojan.cz/">Dalibor Trojan Photography</a></li>
</ul>
<?php
$dirs = DBGalerie::getDirs(true, true);
$dirs[] = array('gd_level' => 1, 'gd_name' => 'Další');

$level_prev = 0;
foreach($dirs as $dir) {
	if($dir['gd_level'] > $level_prev) {
		echo '<ul class="list">', '<li>', $dir['gd_name'];
	} elseif($dir['gd_level'] == $level_prev) {
		echo '</li><li>', $dir['gd_name'];
	} else {
		for($i = 0; $i < ($level_prev - $dir['gd_level']); $i++)
			echo '</li></ul>';
		echo '</li><li>', $dir['gd_name'];
	}
	$level_prev = $dir['gd_level'];
}
for($i = 0; $i < $level_prev; $i++)
	echo '</li></ul>';
?>