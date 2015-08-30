<?php
class ColorBoxHelper {
    protected $popis;
    protected $color;

    public function colorbox($color, $popis)
    {
        $this->color = $color;
        $this->popis = $popis;

        return $this;
    }

    public function render()
    {
        return '<div class="box" ' .
            "title=\"{$this->popis}\" " .
            "style=\"background-color:{$this->color}\"></div>";
    }

    public function __toString()
    {
        return $this->render();
    }
}
