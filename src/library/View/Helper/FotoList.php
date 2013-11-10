<?php
namespace TKOlomouc\View\Helper;

class FotoList
{
    private $_id;
    private $_data;
    private $_subdirs;
    private $_fotky;
    private $_selector;

    function fotoList() {
        $this->_id = -1;
        $this->_data = array();
        $this->_subdirs = array();
        $this->_fotky = array();
        $this->_selector = false;
        return $this;
    }
    function id($id) {
        if ($id >= 0)
            $this->_id = $id;
        return $this;
    }
    function data($data) {
        if (is_array($data))
            $this->_data = $data;
        if ($this->_id == -1)
            $this->_id = $this->_data['gd_id'];
        return $this;
    }
    function subdirs($data) {
        if (is_array($data))
            $this->_subdirs = $data;
        return $this;
    }
    function fotky($data) {
        if (is_array($data))
            $this->_fotky = $data;
        return $this;
    }
    function selector($set) {
        $this->_selector = $set ? true : false;
        return $this;
    }
    function render() {
        if (!$this->_data && !($this->_data = DBGalerie::getSingleDir($this->_id))) {
            return notice('Taková složka neexistuje', true);
        }
        if (empty($this->_subdirs) && !$this->_selector)
            $this->_subdirs = DBGalerie::getSubdirs($this->_id);
        if (empty($this->_fotky))
            $this->_fotky = DBGalerie::getFotky($this->_id);

        $out = '<h2>' . $this->_data['gd_name'] . '</h2>';

        if (empty($this->_fotky) && empty($this->_subdirs)) {
            return $out . notice('Žádné fotky', true);
        }
        if (!$this->_selector) {
            foreach ($this->_subdirs as $item) {
                $out .= '<div class="f"><a href="' . $_SERVER['REQUEST_URI'] . '/' . $item['gd_id'] . '">';
                $out .= '<div class="f_img"><div class="f_preview">';
                $out .= '<img alt="' . $item['gd_id'] . '" src="/images/folder.png" />';
                $out .= '</div></div>';
                $out .= '<div class="f_popis">' . $item['gd_name'] . '</div>';
                $out .= '</a></div>';
            }
        }
        foreach ($this->_fotky as $item) {
            $tn = str_replace('./galerie', '/galerie/thumbnails', $item['gf_path']);

            $out .= '<div class="f">';
            if (!$this->_selector)
                $out .= '<a href="' . $_SERVER['REQUEST_URI'] . '/foto/' . $item['gf_id'] . '">';
            $out .= '<div class="f_img"><div class="f_preview">';
            $out .= '<img alt="' . $item['gf_id'] . '" src="' . $tn . '" />';
            $out .= '</div></div>';
            $out .= '<div class="f_popis">';
            if ($this->_selector)
                $out .= '<input type="checkbox" name="galerie[]" value="' . $item['gf_id'] . '" />';
            $out .= $item['gf_name'];
            $out .= '</div>';
            if (!$this->_selector)
                $out .= '</a>';
            $out .= '</div>';
        }

        return $out;
    }
    function __toString() {
        return $this->render();
    }
}