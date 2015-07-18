<?php
class ClankyHelper
{
    private $_clanky;
    private $_offset;
    private $_number;
    private $_highlights;

    public function clanky() {
        $this->_clanky = null;
        $this->_offset = 0;
        $this->_number = 20;
        $this->_highlights = false;
        return $this;
    }

    public function data($data) {
        $this->_clanky = $data;
        return $this;
    }

    public function offset($o)  {
        $this->_offset = $o;
        return $this;
    }

    public function number($n) {
        $this->_number = $n;
        return $this;
    }

    public function highlights($s) {
        $this->_highlights = $s;
        return $this;
    }

    public function render() {
        if ($this->_clanky === null) {
            $this->_clanky = DBAktuality::getAktuality(AKTUALITY_CLANKY);
        }
        if (($this->_number + $this->_offset) > count($this->_clanky)) {
            $this->_number = count($this->_clanky) - $this->_offset;
        }

        $this->_clanky = array_slice($this->_clanky, $this->_offset, $this->_number);

        $data = array_map(
            function ($val) {
                list($date) = explode(' ', $val['at_timestamp']);
                $photo = DBGalerie::getSingleFoto($val['at_foto_main']);
                $photo_uri = $photo ? $photo['gf_path'] : '';

                return array(
                    'uri'  => '/aktualne/' . $val['at_id'],
                    'name' => $val['at_jmeno'],
                    'date' => formatDate($date),
                    'description' => $val['at_preview'],
                    'title_photo_uri' => '/galerie/' . $photo_uri,
                    'category' => 'Zprávy'
                    //FIXME: Články - kategorie (tagy?)
                );
            },
            $this->_clanky
        );

        if ($this->_highlights) {
            $template = 'files/View/Helper/Highlights.inc';
        } else {
            $template = 'files/View/Helper/ViceClanku.inc';
        }

        $r = new Renderer();
        return $r->render(
            $template,
            array('data' => $data)
        );
   }

    public function __toString()  {
        return $this->render();
    }
}
