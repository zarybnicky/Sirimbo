<?php
class Tag extends Widget
{
    protected $name = '';

    public function __construct($name, array $attributes = array(), $child = array())
    {
        parent::__construct(
            $attributes,
            array_slice(func_get_args(), 2)
        );
        $this->name = $name;
    }

    public function render()
    {
        static $voidTags;
        static $validAttributes;
        if (!$voidTags) {
            $voidTags = array(
                'area', 'base', 'br', 'col', 'command', 'embed', 'hr', 'img',
                'input', 'keygen', 'link', 'meta', 'param', 'source', 'track',
                'wbr', 'basefont', 'bgsound', 'frame', 'isindex'
            );
        }
        if (!$validAttributes) {
            $validAttributes = array(
                'accept', 'acceptCharset', 'accessKey', 'action',
                'allowFullScreen', 'allowTransparency', 'alt', 'async',
                'autoCapitalize', 'autoComplete', 'autoCorrect', 'autoFocus',
                'autoPlay', 'autoSave', 'capture', 'cellPadding',
                'cellSpacing', 'charSet', 'challenge', 'checked', 'classID',
                'class', 'cols', 'colSpan', 'content', 'contentEditable',
                'contextMenu', 'controls', 'coords', 'crossOrigin', 'data',
                'dateTime', 'defer', 'dir', 'disabled', 'download',
                'draggable', 'encType', 'for', 'form', 'formAction',
                'formEncType', 'formMethod', 'formNoValidate', 'formTarget',
                'frameBorder', 'headers', 'height', 'hidden', 'high', 'href',
                'hrefLang', 'httpEquiv', 'icon', 'id', 'inputMode', 'itemProp',
                'itemScope', 'itemType', 'itemRef', 'itemID', 'keyParams',
                'keyType', 'label', 'lang', 'list', 'loop', 'low', 'manifest',
                'marginHeight', 'marginWidth', 'max', 'maxLength', 'media',
                'mediaGroup', 'method', 'min', 'minLength', 'multiple',
                'muted', 'name', 'noValidate', 'open', 'optimum', 'pattern',
                'placeholder', 'poster', 'preload', 'property', 'radioGroup',
                'readOnly', 'rel', 'required', 'results', 'role', 'rows',
                'rowSpan', 'sandbox', 'scope', 'scoped', 'scrolling',
                'seamless', 'selected', 'shape', 'size', 'sizes', 'span',
                'spellCheck', 'src', 'srcDoc', 'srcSet', 'start', 'step',
                'style', 'summary', 'tabIndex', 'target', 'title', 'type',
                'unselectable', 'useMap', 'value', 'width', 'wmode', 'wrap'
            );
        }
        $tag = '<' . $this->name;

        foreach ($this->value as $key => $value) {
            if (!in_array($key, $validAttributes)) {
                continue;
            }
            if ($value === null || $value === false || $value === array()) {
                continue;
            }
            if ($value === true) {
                $value = $key;
            } else {
                if (is_array($value)) {
                    $value = implode(',', $value);
                } else {
                    $value = (string) $value;
                }
                $value = htmlspecialchars($value, ENT_COMPAT | ENT_HTML5, 'UTF-8');
            }

            $tag .= " $key=\"$value\"";
        }
        if (!$this->children && in_array($this->name, $voidTags)) {
            return $tag . ' />';
        }
        $tag .= '>';

        return array(
            $tag,
            $this->getChildren(),
            "</{$this->name}>"
        );
    }
}
