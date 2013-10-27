<?php
class ZpravyHelper {
    private $zpravy;
    private $number;
    
    public function zpravy() {
        $this->_defaultValues();
        return $this;
    }
    private function _defaultValues() {
        $this->zpravy = null;
    }
    public function data($data = null) {
        if($data !== null)
            $this->zpravy = $data;
        return $this;
    }
    public function number($n = null) {
        if($n !== null)
            $this->number = $n;
        return $this;
    }
    public function render() {
        if($this->zpravy === null)
            $this->zpravy = DBAktuality::getAktuality(AKTUALITY_KRATKE);
        if($this->number > count($this->zpravy))
            $this->number = count($this->zpravy);
        
        $out = '<div class="zpravy" style="width:250px;padding:5px;height:396px;border:1px solid #FFB030;border-radius:15px;">';
        $out .= '<div class="h_section">Krátké zprávy</div>';
        $out .= '<div style="overflow-y:scroll;padding:4px;height:345px;">';
        if($this->number === 0)
            $out .= '<div class="notice">Žádné zprávy</div>';
        else
            for($i = 0; $i < $this->number; $i++) {
                if(!isset($this->zpravy[$i]))
                    continue;
                
                $out .= '<div>';
                list($date, $time) = explode(' ', $this->zpravy[$i]['at_timestamp']);
                $out .= '<span class="big">' . $this->zpravy[$i]['at_jmeno'] . '</span>' .
                    '<span class="little">&nbsp;(' . formatDate($date) . ')</span>';
                $out .= '<p style="text-align:left;">' . $this->zpravy[$i]['at_text'] . '</p>';    
                $out .= '</div>';
                if(isset($this->zpravy[$i + 1]))
                    $out .= '<hr/>';
            }
        $out .= '</div>';
        $out .= '</div>';
        
        return $out;
    }
    public function __toString()  {
        return $this->render();
    }
}