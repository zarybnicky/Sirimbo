<?php
class ClankyHelper
{
    private $_clanky;
    private $_offset;
    private $_number;
    private $isSlideBox;

    public function clanky() {
        $this->_defaultValues();
        return $this;
    }
    private function _defaultValues() {
        $this->_clanky = null;
        $this->offset = 0;
        $this->_number = 20;
        $this->isSlideBox = false;
    }
    public function data($data = null) {
        if ($data !== null)
            $this->_clanky = $data;
        return $this;
    }
    public function offset($o = null)  {
        if ($o !== null)
            $this->offset = $o;
        return $this;
    }
    public function number($n = null) {
        if ($n !== null)
            $this->_number = $n;
        return $this;
    }
    public function slideBox($s = null) {
        if ($s !== null)
            $this->isSlideBox = $s;
        return $this;
    }
    public function render() {
        if ($this->_clanky === null)
            $this->_clanky = DBAktuality::getAktuality(AKTUALITY_CLANKY);
        if (($this->_number + $this->offset) > count($this->_clanky))
            $this->_number = count($this->_clanky) - $this->offset;
        $out = '';

        if (!$this->isSlideBox) {
            $out .= '<div class="clanky" style="padding:4px 7px;border:1px solid #FFB030;border-radius:15px;">';
            $out .= '<div class="h_section">Další články</div>';
            if ($this->_number <= 0)
                $out .= '<div class="notice">Žádné další články</div>';
            for($i = $this->offset; $i < ($this->offset + $this->_number); $i++) {
                $out .= '<div>';
                list($date, $time) = explode(' ', $this->_clanky[$i]['at_timestamp']);
                $out .= '<span class="big">' .
                    '<a href="/aktualne/' . $this->_clanky[$i]['at_id'] . '">' .
                    $this->_clanky[$i]['at_jmeno'] .
                    '</a></span>' .
                    '<span class="little">&nbsp;(' . formatDate($date) . ')</span>';
                $out .= '<p>' . $this->_clanky[$i]['at_preview'] . '...</p>';
                $out .= '</div>';
                if (isset($this->_clanky[$i + 1]))
                    $out .= '<hr/>';
            }
            $out .= '</div>';
        } elseif ($this->isSlideBox && $this->_number > 0) {
            $out .= '<div id="TopFeaturesBlock">';
            for($i = $this->offset; $i < ($this->offset + $this->_number); $i++) {
                $out .= '<div class="Feature"' . (($i != $this->offset) ? ' style="display:none;"' : '') . '>';
                $out .= '<h2 class="FeatureTitle">' . $this->_clanky[$i]['at_jmeno'] . '</h2>';
                $out .= '<div class="FeaturePhoto">';
                $foto = DBGalerie::getSingleFoto($this->_clanky[$i]['at_foto_main']);
                if ($foto['gf_path'])
                    $out .= '<img src="' . $foto['gf_path'] . '" alt="' . $this->_clanky[$i]['at_jmeno'] . '" />';
                $out .= '<p></p>';
                $out .= '</div>';
                $out .= '<p class="FeatureBlurb">' . $this->_clanky[$i]['at_preview'] . '...</p>';
                $out .= '<ul class="FeatureLinks">';
                $out .= '<li><a href="/aktualne/' . $this->_clanky[$i]['at_id'] . '">Více</a></li>';
                $out .= '</ul>';
                $out .= '<div style="clear:both;"></div>';
                $out .= '</div>';
            }
            $out .= '</div>';
            ?>
            <script type="text/javascript">
            (function($) {
                $(function() {
                    if (typeof $.fn.topFeatures == "undefined") {
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