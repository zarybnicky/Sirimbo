<?php
class FotoListHelper {
    private $id;
    private $data;
    private $subdirs;
    private $fotky;
    private $selector;
    
    function fotoList() {
        $this->id = -1;
        $this->data = array();
        $this->subdirs = array();
        $this->fotky = array();
        $this->selector = false;
        return $this;
    }
    function id($id) {
        if($id >= 0)
            $this->id = $id;
        return $this;
    }
    function data($data) {
        if(is_array($data))
            $this->data = $data;
        if($this->id == -1)
            $this->id = $this->data['gd_id'];
        return $this;
    }
    function subdirs($data) {
        if(is_array($data))
            $this->subdirs = $data;
        return $this;
    }
    function fotky($data) {
        if(is_array($data))
            $this->fotky = $data;
        return $this;
    }
    function selector($set) {
        $this->selector = $set ? true : false;
        return $this;
    }
    function render() {
        if(!$this->data && !($this->data = DBGalerie::getSingleDir($this->id))) {
            return notice('Taková složka neexistuje', true);
        }
        if(empty($this->subdirs) && !$this->selector)
            $this->subdirs = DBGalerie::getSubdirs($this->id);
        if(empty($this->fotky))
            $this->fotky = DBGalerie::getFotky($this->id);
        
        $out = '<h2>' . $this->data['gd_name'] . '</h2>';
        
        if(empty($this->fotky) && empty($this->subdirs)) {
            return $out . notice('Žádné fotky', true);
        }
        if(!$this->selector) {
            foreach($this->subdirs as $item) {
                $out .= '<div class="f"><a href="' . $_SERVER['REQUEST_URI'] . '/' . $item['gd_id'] . '">';
                $out .= '<div class="f_img"><div class="f_preview">';
                $out .= '<img alt="' . $item['gd_id'] . '" src="/images/folder.png" />';
                $out .= '</div></div>';
                $out .= '<div class="f_popis">' . $item['gd_name'] . '</div>';
                $out .= '</a></div>';
            }
        }
        foreach($this->fotky as $item) {
            $tn = str_replace('./galerie', '/galerie/thumbnails', $item['gf_path']);
            
            $out .= '<div class="f">';
            if(!$this->selector)
                $out .= '<a href="' . $_SERVER['REQUEST_URI'] . '/foto/' . $item['gf_id'] . '">';
            $out .= '<div class="f_img"><div class="f_preview">';
            $out .= '<img alt="' . $item['gf_id'] . '" src="' . $tn . '" />';
            $out .= '</div></div>';
            $out .= '<div class="f_popis">';
            if($this->selector)
                $out .= '<input type="checkbox" name="galerie[]" value="' . $item['gf_id'] . '" />';
            $out .= $item['gf_name'];
            $out .= '</div>';
            if(!$this->selector)
                $out .= '</a>';
            $out .= '</div>';
        }
        
        return $out;
    }
    function __toString() {
        return $this->render();
    }
}