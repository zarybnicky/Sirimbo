<?php
class ClankyHelper {
	private $clanky;
	private $offset;
	private $number;
	private $isSlideBox;
	
	public function clanky() {
		$this->_defaultValues();
		return $this;
	}
	private function _defaultValues() {
		$this->clanky = null;
		$this->offset = 0;
		$this->number = 20;
		$this->isSlideBox = false;
	}
	public function data($data = null) {
		if($data !== null)
			$this->clanky = $data;
		return $this;
	}
	public function offset($o = null)  {
		if($o !== null)
			$this->offset = $o;
		return $this;
	}
	public function number($n = null) {
		if($n !== null)
			$this->number = $n;
		return $this;
	}
	public function slideBox($s = null) {
		if($s !== null)
			$this->isSlideBox = $s;
		return $this;
	}
	public function render() {
		if($this->clanky === null)
			$this->clanky = DBAktuality::getAktuality(AKTUALITY_CLANKY);
		if(($this->number + $this->offset) > count($this->clanky))
			$this->number = count($this->clanky) - $this->offset;
		$out = '';
		
		if(!$this->isSlideBox) {
			$out .= '<div class="clanky" style="padding:4px 7px;border:1px solid #FFB030;border-radius:15px;">';
			$out .= '<div class="h_section">Další články</div>';
			if($this->number <= 0)
				$out .= '<div class="notice">Žádné další články</div>';
			for($i = $this->offset; $i < ($this->offset + $this->number); $i++) {
				$out .= '<div>';
				list($date, $time) = explode(' ', $this->clanky[$i]['at_timestamp']);
				$out .= '<span class="big">' .
					'<a href="/aktualne/' . $this->clanky[$i]['at_id'] . '">' .
					$this->clanky[$i]['at_jmeno'] .
					'</a></span>' .
					'<span class="little">&nbsp;(' . formatDate($date) . ')</span>';
				$out .= '<p>' . $this->clanky[$i]['at_preview'] . '...</p>';	
				$out .= '</div>';
				if(isset($this->clanky[$i + 1]))
					$out .= '<hr/>';
			}
			$out .= '</div>';
		} elseif($this->isSlideBox && $this->number > 0) {
			$out .= '<div id="TopFeaturesBlock">';
			for($i = $this->offset; $i < ($this->offset + $this->number); $i++) {
				$out .= '<div class="Feature"' . (($i != $this->offset) ? ' style="display:none;"' : '') . '>';
				$out .= '<h2 class="FeatureTitle">' . $this->clanky[$i]['at_jmeno'] . '</h2>';
				$out .= '<div class="FeaturePhoto">';
				$foto = DBGalerie::getSingleFoto($this->clanky[$i]['at_foto_main']);
				$out .= '<img src="' . $foto['gf_path'] . '" alt="" />';
				$out .= '<p></p>';
				$out .= '</div>';
				$out .= '<p class="FeatureBlurb">' . $this->clanky[$i]['at_preview'] . '...</p>';
				$out .= '<ul class="FeatureLinks">';
				$out .= '<li><a href="/aktualne/' . $this->clanky[$i]['at_id'] . '">Více</a></li>';
				$out .= '</ul>';
				$out .= '<div style="clear:both;"></div>';
				$out .= '</div>';
			}
			$out .= '</div>';
			?>
			<script type="text/javascript">
			(function($) {
				$(function() {
					if(typeof $.fn.topFeatures == "undefined") {
						$.getScript("/scripts/topFeatures.min.js", function() {
							$("#TopFeaturesBlock").topFeatures()
						});
					} else {
						$("#TopFeaturesBlock").topFeatures();
					}
				})
			})(jQuery);
			</script>
			<?php
		}
		return $out;
	}
	public function __toString()  {
		return $this->render();
	}
}