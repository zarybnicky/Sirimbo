<?php
class Controller_Kontakt implements Controller_Interface {
    function view($id = null) {
        header_main('Kontakty');
        ?>
<style type="text/css">
table.k {border:none;}
table.k td {border:none;background:none;}
table.k td.r {padding-right:10px;text-align:right;}
table.k b {color:#572E00;}
</style>
<table class="k">
<tr><td><?php header_minor('Taneční klub Olymp Olomouc');?></td></tr>
<tr>
	<td class="r">sídlo:</td>
	<td>Jiráskova 25, 77900, Olomouc</td>
</tr>
<tr>
	<td class="r">IČO:</td>
	<td>68347286</td>
</tr>
<tr>
	<td class="r">bankovní spojení:</td>
	<td>1806875329/0800</td>
</tr>
<tr><td><?php header_minor('Klubová rada');?></td></tr>
<tr>
	<td class="r"><b>Mgr. Marie Hýžová</b></td>
	<td><b>předseda</b></td>
</tr>
<tr>
	<td class="r">mobil:</td>
	<td>604 756 085</td>
</tr>
<tr>
	<td class="r">email:</td>
	<td>tkolymp(zavináč)tkolymp.cz</td>
</tr>
<tr><td>&nbsp;</td><td></td></tr>
<tr>
	<td class="r"><b>Miroslav Hýža</b></td>
	<td><b>místopředseda</b></td>
</tr>
<tr>
	<td></td><td>kontaktní osoba pro informace, tréninky,<br/>vystoupení, soutěže, nové zájemce</td>
</tr>
<tr>
	<td class="r">mobil:</td>
	<td>737 545 525</td>
</tr>
<tr>
	<td class="r">email:</td>
	<td>hyzam(zavináč)tkolymp.cz</td>
</tr>
<tr><td>&nbsp;</td><td></td></tr>
<tr>
	<td class="r"><b>Hana Jendrulková</b></td>
	<td><b>hospodář</b></td>
</tr>
<tr>
	<td class="r">mobil</td>
	<td>606 426 614</td>
</tr>
<tr>
	<td class="r">email:</td>
	<td>h.jendrulkova(zavináč)tkolymp.cz</td>
</tr>
<tr><td>&nbsp;</td><td></td></tr>
<tr><td><?php header_minor('Dozorčí rada');?></td></tr>
<tr>
	<td class="r"><b>Bc. Marek Černý</b></td>
</tr>
<tr>
	<td class="r">mobil</td>
	<td>605 787 749</td>
</tr>
<tr>
	<td class="r">email:</td>
	<td>marek(zavináč)tanecni-olomouc.cz</td>
</tr>
<tr><td>&nbsp;</td><td></td></tr>
<tr>
	<td class="r"><b>Martin Tonner</b></td>
</tr>
<tr>
	<td class="r">mobil</td>
	<td>603 230 972</td>
</tr>
<tr>
	<td class="r">email:</td>
	<td>martin.to(zavináč)seznam.cz</td>
</tr>
<tr><td>&nbsp;</td><td></td></tr>
<tr>
	<td class="r"><b>Vít Spurný</b></td>
</tr>
<tr>
	<td class="r">mobil</td>
	<td>728 887 084</td>
</tr>
<tr>
	<td class="r">email:</td>
	<td>vit.spurny(zavináč)gmail.com</td>
</tr>
</table>
<?php
    }
}
?>