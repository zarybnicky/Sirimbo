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
    }

    public function __toString()  {
        return $this->render();
    }
}