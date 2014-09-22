<?php
class ZpravyHelper
{
    private $_zpravy;
    private $_number;

    public function zpravy() {
        $this->_defaultValues();
        return $this;
    }
    private function _defaultValues() {
        $this->_zpravy = null;
    }
    public function data($data = null) {
        if ($data !== null)
            $this->_zpravy = $data;
        return $this;
    }
    public function number($n = null) {
        if ($n !== null) {
            $this->_number = $n;
        }
        return $this;
    }
    public function render() {
        if ($this->_zpravy === null) {
            $this->_zpravy = DBAktuality::getAktuality(AKTUALITY_KRATKE);
        }
        if ($this->_number > count($this->_zpravy)) {
            $this->_number = count($this->_zpravy);
        }

        $data = array_map(
            function ($val) {
                return array(
                    'text' => $val['at_text']
                );
            },
            $this->_zpravy
        );

        $r = new Renderer();
        return $r->render(
            'files/View/Helper/Zpravy.inc',
            array('data' => $data)
        );

/*
        $out = '<div class="zpravy">';
        $out .= '<div style="overflow-y:scroll;padding:4px;height:345px;">';
        if ($this->_number === 0) {
            $out .= '<div class="notice">Žádné zprávy</div>';
        } else {
            for($i = 0; $i < $this->_number; $i++) {
                if (!isset($this->_zpravy[$i]))
                    continue;

                $out .= '<div>';
                list($date, $time) = explode(' ', $this->_zpravy[$i]['at_timestamp']);
                $out .= '<span class="big">' . $this->_zpravy[$i]['at_jmeno'] . '</span>' .
                    '<span class="little">&nbsp;(' . formatDate($date) . ')</span>';
                $out .= '<p style="text-align:left;">' . $this->_zpravy[$i]['at_text'] . '</p>';
                $out .= '</div>';
                if (isset($this->_zpravy[$i + 1]))
                    $out .= '<hr/>';
            }
        }
        $out .= '</div>';
        $out .= '</div>';

        return $out;
*/
    }
    public function __toString()  {
        return $this->render();
    }
}