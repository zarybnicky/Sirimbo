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
<style>
.fotolist {
	margin: 0;
	padding: 0;
}
.fotolist li {
	list-style: none;
	padding-left:15px;
	margin: 3px 0;
} .fotolist img {
	display: inline;
	margin: 3px 5px -3px 0;
}
</style>
<?php
$dirs = DBGalerie::getDirs(true, true);
$dirs[] = array('gd_level' => 2, 'gd_id' => '#', 'gd_name' => 'Další');

$level_prev = 0;
foreach($dirs as $dir) {
	if($dir['gd_level'] > $level_prev) {
		if($level_prev != 0)
			echo '</a>';
		echo '<ul class="fotolist">';
		echo '<li><a href="', $dir['gd_id'], '"><img src="/style/directory.png" alt="" />', $dir['gd_name'];
	} elseif($dir['gd_level'] == $level_prev) {
		echo '</a></li>';
		echo '<li><a href="', $dir['gd_id'], '"><img src="/style/directory.png" alt="" />', $dir['gd_name'];
	} else {
		for($i = 0; $i < ($level_prev - $dir['gd_level']); $i++)
			echo '</a></li></ul>';
		echo '</a></li>';
		echo '<li><a href="', $dir['gd_id'], '"><img src="/style/directory.png" alt="" />', $dir['gd_name'];
	}
	$level_prev = $dir['gd_level'];
}
for($i = 0; $i < $level_prev; $i++)
	echo '</a></li></ul>';
?>