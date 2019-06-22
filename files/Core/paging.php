<?php
class Paging
{
    private $_previousPage;
    private $_currentPage;
    private $_nextPage;
    private $_itemsPerPage;
    private $_defaultItemsPerPage = 20;
    private $_itemsPerPageField = 'c';
    private $_currentPageField = 'p';
    private $_pageRange = 5;
    private $_pageCount;
    private $_totalItems;
    private $_pagesInRange;
    private $_valid = false;
    private $_source;

    public function __construct($source, $options = null)
    {
        if (!in_array('Pagable', class_implements($source))) {
            throw new ViewException('Database does not implement interface Pageable');
        }
        $this->_source = $source;
        $this->_sourceOpts = $options;
        $this->_totalItems = call_user_func([$source, 'getCount'], $options);
        $this->_recalculate();
        return $this;
    }

    public function setCurrentPage($p)
    {
        if (!is_numeric($p)) {
            return false;
        }
        $this->_currentPage = $p;
        $this->_recalculate();
    }

    public function setItemsPerPage($p)
    {
        if (!is_numeric($p)) {
            return false;
        }
        $this->_itemsPerPage = $p;
        $this->_recalculate();
    }

    public function setDefaultItemsPerPage($i)
    {
        if (!is_numeric($i)) {
            return false;
        }
        $this->_defaultItemsPerPage = $i;
        $this->_recalculate();
    }

    public function setPageRange($p)
    {
        if (is_numeric($p)) {
            $this->_pageRange = $p;
        }
        $this->_recalculate();
    }

    private function _recalculate()
    {
        if (!isset($this->_source)) {
            $this->_valid = false;
            return;
        }

        if (!$this->_itemsPerPage) {
            $this->_itemsPerPage = $this->_defaultItemsPerPage ?: 20;
        }

        if (!$this->_currentPage) {
            $this->_currentPage = 1;
        }

        $this->_pageCount = ceil($this->_totalItems / $this->_itemsPerPage);
        if ($this->_currentPage < 1) {
            $this->_currentPage = 1;
        } elseif ($this->_currentPage > $this->_pageCount && $this->_pageCount > 0) {
            $this->_currentPage = $this->_pageCount;
        }

        $this->_previousPage = max(1, $this->_currentPage - 1);
        $this->_nextPage = min($this->_currentPage + 1, max($this->_pageCount, 1));
        $this->_pagesInRange = [];

        if (isset($this->_pageRange)) {
            $halfRange = floor($this->_pageRange / 2);

            if ($this->_pageRange > $this->_pageCount) {
                $this->_pagesInRange = range(1, $this->_pageCount);
            } elseif ($this->_currentPage - $halfRange <= 1) {
                $this->_pagesInRange = range(1, $this->_pageRange);
            } elseif ($this->_currentPage + $halfRange >= $this->_pageCount) {
                $this->_pagesInRange = range(
                    $this->_pageCount - $this->_pageRange + 1,
                    $this->_pageCount
                );
            } else {
                $this->_pagesInRange = range(
                    $this->_currentPage - $halfRange,
                    $this->_currentPage + $halfRange
                );
            }
        } else {
            $this->_pagesInRange = range(1, $this->_pageCount);
        }

        $this->_valid = true;
    }

    public function getPages()
    {
        if (!$this->_valid) {
            return false;
        }
        return [
            'first' => 1,
            'previous' => $this->_previousPage,
            'current' => $this->_currentPage,
            'next' => $this->_nextPage,
            'last' => $this->_pageCount,
            'total' => $this->_totalItems,
            'pagesInRange' => $this->_pagesInRange
        ];
    }

    public function getLink($get, $i, $label, $perPage = null)
    {
        if (!$this->_valid) {
            return false;
        }
        if ($perPage === null) {
            $perPage = $this->_itemsPerPage;
        }

        $p = $this->_currentPageField ?: 'p';
        $c = $this->_itemsPerPageField ?: 'c';
        $url = http_build_query(array_merge($get, [$c => $perPage, $p => $i]));
        return "<a href=\"?$url\">$label</a>";
    }

    public function getNavigation($get)
    {
        if ($this->_pageCount <= 1) {
            return $this->getCountSetting($get);
        }

        $out = '';
        if ($this->getCurrentPage() != 1) {
            $out .= $this->getLink($get, 1, '&lt;&lt;') . '&nbsp;';
            $out .= $this->getLink($get, $this->_previousPage, '&lt;');
        } else {
            $out .= '<span style="padding:0 2px;">&lt;&lt;</span>&nbsp;';
            $out .= '<span style="padding:0 2px;">&lt;</span>';
        }
        $pages = $this->_pagesInRange;
        if (end($pages) + 10 < $this->_pageCount) {
            array_push($pages, end($pages) + 10);
        }
        if (reset($pages) - 10 > 1) {
            array_unshift($pages, reset($pages) - 10);
        }

        $out .= '&nbsp;|';
        foreach ($pages as $i) {
            $out .= '<div style="text-align:center;width:3ex;display:inline-block;*display:inline;">';
            if ($i != $this->getCurrentPage()) {
                $out .= $this->getLink($get, $i, $i);
            } else {
                $out .= $i;
            }
            $out .= '</div>|';
        }
        $out .= '&nbsp;';
        if ($this->getCurrentPage() != $this->_pageCount) {
            $out .= $this->getLink($get, $this->_nextPage, '&gt;') . '&nbsp;';
            $out .= $this->getLink($get, $this->_pageCount, '&gt;&gt;');
        } else {
            $out .= '<span style="padding:0 2px;">&gt;</span>&nbsp;';
            $out .= '<span style="padding:0 2px;">&gt;&gt;</span>';
        }
        return $out . '<br/>' . $this->getCountSetting($get);
    }

    public function getCountSetting($get)
    {
        if ($this->_totalItems <= 5) {
            return '';
        }
        $options = [5, 10, 20, 50];

        $out = 'zobrazit ';
        if ($this->_itemsPerPage === $this->_totalItems) {
            $out .= '<span style="padding:0 2px;">vše</span>';
        } else {
            $out .= $this->getLink($get, 1, 'vše', $this->_totalItems);
        }

        foreach ($options as $option) {
            if ($option > $this->_totalItems) {
                continue;
            }

            if ($this->_itemsPerPage == $option) {
                $out .= '&nbsp;|&nbsp;<span style="padding:0 2px;">' . $option . '</span>';
            } else {
                $i = floor($this->_itemsPerPage * ($this->_currentPage - 1) / $option) + 1;
                $out .= '&nbsp;|&nbsp;' . $this->getLink($get, $i, $option, $option);
            }
        }
        $out .= ' položek na stránku';
        return $out;
    }

    public function getCurrentPage() {
        if (!$this->_valid) {
            return false;
        }
        return $this->_currentPage;
    }

    public function getItemsPerPage() {
        if (!$this->_valid) {
            return false;
        }
        return $this->_itemsPerPage;
    }

    public function getItems()
    {
        if (!$this->_valid) {
            return [];
        }
        return call_user_func(
            [$this->_source, 'getPage'],
            $this->_itemsPerPage * ($this->_currentPage - 1),
            $this->_itemsPerPage,
            $this->_sourceOpts
        );
    }
}
