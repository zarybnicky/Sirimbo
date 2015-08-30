<?php
class MenuHelper
{
    protected $content;

    public function menu()
    {
        $this->content = array();
        return $this;
    }

    public function content($name, $url, $button = false, $replace = false)
    {
        if ($replace) {
            $this->content = array();
        }
        $this->content[] = array($name, $url, $button);

        return $this;
    }

    public function render()
    {
        if (!$this->content) {
            return '';
        }

        $out = '<div class="sticky" style="width:150px;float:right;">';
        ?>
        <script type="text/javascript">
        (function($) {
            $(function() {
                if (typeof $.fn.sticky == "undefined") {
                    $.getScript("/scripts/jquery.sticky.js", function() {
                        $(".sticky").sticky({topSpacing:15});
                    });
                } else {
                    $(".sticky").sticky({topSpacing:15});
                }
            })
        })(jQuery);
        </script>
        <?php
        $out .= '<div style="z-index:100;width:inherit;' .
            'border:1px solid #aaa;background:#ddd;margin-top:2px;padding:3px 1px;">';

        $content = array_map(
            function ($data) {
                list($name, $url, $button) = $data;
                if ($button) {
                    return '<button style="padding:0" name="action" value="' . $url . '">' . $name . '</button>';
                } else {
                    return '<a style="padding:0 3px" href="' . $url . '">' . $name . '</a>';
                }
            },
            $this->content
        );
        $out .= implode('<br/>', $content);

        $out .= '</div>';
        $out .= '</div>';
        return $out;
    }
    public function __toString()
    {
        return $this->render();
    }
}